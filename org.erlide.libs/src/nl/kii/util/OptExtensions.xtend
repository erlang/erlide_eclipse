package nl.kii.util

class OptExtensions {
	
	// BOOLEAN DEFINED CHECK //////////////////////////////////////////////////

	/**
	 * Checks if an object is defined, meaning here that it is not empty or faulty
	 */
	def static <T> defined(Object o) {
		switch(o) {
			case null: false
			None<T>: false
			Err<T>: false
			default: true
		}		
	}

	/**
	 * Only perform the function for a given condition. Returns an optional result.
	 * <pre>val Opt<User> user = ifTrue(isMale) [ getUser ]</pre>
	 */
	def static <T, I> Opt<T> ifTrue(boolean condition, (Object)=>T fn) {
		if(condition) fn.apply(null).option
		else none
	}

	/**
	 * Only perform the function if something was set. Returns an optional result.
	 * <pre>val Opt<User> user = ifSome(userId) [ getUser(userId) ]</pre>
	 */
	def static <T, I> Opt<T> ifSome(Opt<I> o, (I)=>T fn) {
		if(o.defined) fn.apply(o.value).option
		else none
	}

	/**
	 * Only perform the function if the passed argument was empty,
	 * meaning null or empty or an error. Returns an optional result.
	 * <pre>val Opt<User> user = ifEmpty(userId) [ getDefaultUser ]</pre>
	 */
	def static <T, I> Opt<T> ifEmpty(Opt<I> o, (I)=>T fn) {
		if(!o.defined) fn.apply(o.value).option
		else none
	}

	/**
	 * Only perform the function if the passed argument was an error result.
	 * Returns an optional result.
	 * <pre>
	 * val Opt<User> user = getUserOpt(userId)
	 * val Opt<String> warning = ifError(user) [ 'something went wrong!' ]
	 * </pre>
	 */
	def static <T, I> Opt<T> ifError(Opt<I> o, (I)=>T fn) {
		if(o.hasError) fn.apply(o.value).option
		else none
	}
	
	// OPTION CREATION ////////////////////////////////////////////////////////
	
	/**
	 * Create an option from an object. It detect if it is a None or a Some.
	 * <pre>api.getUser(userId).option // if getUser returns null, it will be None, otherwise Some<User></pre>
	 */
	def static <T> Opt<T> option(T value) {
		if(value.defined) some(value) else none
	}
	
	def static <T> Some<T> some(T value) {
		new Some<T>(value)
	}
	
	def static <T> None<T> none() {
		new None<T>
	}
	
	def static <T> Err<T> error(Throwable t) {
		new Err<T>(t)
	}
	
	def static <T> Err<T> error() {
		new Err<T>()
	}

	// ATTEMPTS ///////////////////////////////////////////////////////////////

	/**
	 * wrap a call as an option (exception or null generates none)<p>
	 * example: val userOption = attempt [ api.getUser(userId) ] // if API throws exception, return None
	 */
	def static <T> Opt<T> attempt((Object)=> T fn) {
		try {
			fn.apply(null).option
		} catch (Exception e) {
			error(e)
		}
	}
	
	/** 
	 * Same as => but with optional execution and option result<p>
	 * example: normally you do: user => [ name = 'john' ]<p>
	 * but what if user is of type Option<User><p>
	 * then you can do: user.attempt [ name = 'john' ]<br>
	 * the assignment will only complete if there was a user
	 */
	def static <T, O> Opt<O> attempt(Opt<O> o, (O)=>T fn) {
		if(o.defined) fn.apply(o.value)
		o
	}

	/**
	 * Same as => but with optional execution and option result
	 * example: normally you do: user => [ name = 'john' ]
	 * but what if user is of type Option<User>
	 * then you can do: user.attempt [ name = 'john' ]
	 * the assignment will only complete if there was a user
	 * <p>
	 * This version accept functions that have no result
	 */
	def static <T, O> Opt<O> attempt(Opt<O> o, (O)=>void fn) {
		if(o.defined) fn.apply(o.value)
		o
	}

	// MAPPING ////////////////////////////////////////////////////////////////

	def static <I, T> I apply(I input, (I)=>void fn) {
		fn.apply(input)
		input
	}

	/** 
	 * Transform an option into a new option using a function.
	 * The function allows you to transform the value of the passed option,
	 * saving you the need to unwrap it yourself
	 */
	def static <T, I> Opt<T> mapOpt(Opt<I> o, (I)=>T fn) {
		if(o.defined) fn.apply(o.value).option else none
	}

	// OPTIONAL FALLBACK EXTENSIONS ///////////////////////////////////////////
	
	/** 
	 * Provide a fallback value if o is undefined
	 *  <pre>val user = foundUser.or(defaultUser)</pre>
	 */
	def static <T> T or(T o, T fallback) {
		if(o.defined) o else fallback
	}

	/**
	 * provide a fallback value if o is undefined
	 * <pre>val user = api.getUser(12).or(defaultUser) // getUser returns an Option<User></pre>
	 */
	def static <T> T or(Opt<T> o, T fallback) {
		if(o.defined) o.value else fallback
	}

	// run a fallback function if o is undefined
	// example: val user = foundUser.or [ api.getDefaultUser() ]
	def static <T> T or(T o, (Object)=>T fallbackFn) {
		if(o.defined) o else fallbackFn.apply(null)
	}
	
	// run a fallback function if o is undefined
	// example: val user = api.getUser(12).or [ api.getDefaultUser() ] // getUser returns an Option<User>
	def static <T> T or(Opt<T> o, (Object)=>T fallbackFn) {
		if(o.defined) o.value else fallbackFn.apply(null)
	}

	def static <T> T orNull(T o) {
		if(o.defined) o else null
	}
	
	def static <T> T orNull(Opt<T> o) {
		if(o.defined) o.value else null
	}
	
	def static <T> T orThrow(Opt<T> o) {
		switch(o) {
			Err<T>: throw o.exception
			None<T>: throw new NoneException
			Some<T>: o.value
		}
	}

	// throw the result of the exceptionFn if o is not defined
	// example: val user = api.getUser(12).orThrow [ new UserNotFoundException ] // getUser can return null
	def static <T> T orThrow(T o, (Object)=>Throwable exceptionFn) {
		if(o.defined) o else throw exceptionFn.apply(null)
	}

	// try to unwrap an option, and if there is nothing, calls the exceptionFn to get a exception to throw
	// example: val user = api.getUser(12).orThrow [ new UserNotFoundException ] // getUser returns an option
	def static <T> T orThrow(Opt<T> o, (Object)=>Throwable exceptionFn) {
		if(o.defined) o.value else throw exceptionFn.apply(null)
	}	
	
	
	
}
