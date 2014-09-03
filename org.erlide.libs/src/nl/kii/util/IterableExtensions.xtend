package nl.kii.util

import com.google.common.collect.ImmutableList
import com.google.common.collect.ImmutableSet
import java.util.HashMap
import java.util.HashSet
import java.util.List
import java.util.Map
import java.util.Set

import static extension nl.kii.util.OptExtensions.*

class IterableExtensions {
	
	// CREATING (immutable) ///////////////////////////////////////////////////
	
	def static <T> List<T> list(Class<T> cls) {
		newImmutableList
	}

	def static <T> List<T> list(T... objects) {
		newImmutableList(objects)
	}
	
	// CONVERTING (immutable) /////////////////////////////////////////////////
	
	/** Always returns an immutable list, even if a null result is passed. handy when chaining, eliminates null checks
	 * <pre>example: getUsers.filter[age>20].list</pre>
	 */
	def static <T> List<T> toList(Iterable<T> iterable) {
		if(!iterable.defined) newImmutableList
		else iterable.iterator.toList.immutableCopy
	}

	/** Always returns an immutable set, even if a null result was passed. handy when chaining, eliminates null checks.	
	 * note: double values will be removed!
	 */
	def static <T> Set<T> toSet(Iterable<T> iterable) {
		if(!iterable.defined) newHashSet else {
			val uniques = iterable.distinct.toList
			new HashSet<T>(uniques).immutableCopy
		}
	}

	// ADDING (immutable) /////////////////////////////////////////////////////
	
	def static <T> addSafe(Iterable<T> list, T value) {
		ImmutableList.builder.addAll(list).add(value).build
	}
	
	def static <T> addSafe(Set<T> set, T value) {
		ImmutableSet.builder.addAll(set).add(value).build
	}

	// SIDEEFFECTS ////////////////////////////////////////////////////////////

	def static <T> Iterable<T> each(Iterable<T> iterable, (T)=>void fn) {
		iterable.forEach(fn)
		iterable
	}
	
	/** Triggers the passed or function for each none in the list,
	 * handy for tracking empty results, for example:
	 * <pre>usersIds.map [ get user ].onNone [ println('could not find user') ].filterNone</pre>
	 */
	def static <T> Iterable<? extends Opt<T>> onNone(Iterable<? extends Opt<T>> iterable, (None<T>)=>void noneHandler) {
		iterable.filter [ hasNone ].each [ noneHandler.apply(it as None<T>) ]
		iterable
	}

	/** Triggers the passed or function for each error in the list,
	 * handy for tracking errors for example:
	 * <pre>usersIds.attemptMap [ get user ].onError [ error handling ].filterEmpty</pre>
	 */
	def static <T> Iterable<? extends Opt<T>> onError(Iterable<? extends Opt<T>> iterable, (Err<T>)=>void errorHandler) {
		iterable.filter [ hasError ].each [ errorHandler.apply(it as Err<T>) ]
		iterable
	}

	// FILTERING //////////////////////////////////////////////////////////////

	/** Convert a list of options into actual values, filtering out the none and error values.
		Like filterNull, but then for a list of Options */
	def static <T> Iterable<T> filterEmpty(Iterable<? extends Opt<T>> iterable) {
		iterable.map [ orNull ].filterNull
	}
	
	def static <T> Iterable<? extends Opt<T>> filterError(Iterable<? extends Opt<T>> iterable) {
		iterable.filter [ !hasError ]
	}

	/** Remove all double values in a list, turning it into a list of unique values */
	def static <T> Iterable<T> distinct(Iterable<T> values) {
		values
			.groupBy[it]
			.toPairs
			.map [ value.head ]
	} 

	// MAPPING ////////////////////////////////////////////////////////////////

	def static <T, R> Iterable<Opt<R>> mapOpt(Iterable<Opt<T>> iterable, (T)=>R fn) {
		iterable.map [ mapOpt(fn) ]
	}
	
	// try to map the values in the iterable, and give back a list of options instead of direct values
	def static <T, R> Iterable<Opt<R>> attemptMap(Iterable<T> iterable, (T)=>R fn) {
		iterable.map [
			val o = it
			attempt [ fn.apply(o) ]
		]
	}
	
	// transforms a map into a list of pairs
	def static <K, V> Iterable<Pair<K, V>> toPairs(Map<K, V> map) {
		map.entrySet.map [ it.key -> it.value ]
	}
	
	// convert a list of pairs to a map
	def static <K, V> Map<K, V> toMap(Iterable<Pair<K, V>> pairs) {
		val map = newHashMap
		if(pairs.defined) pairs.forEach [ map.put(key, value) ]
		map
	}
	
	// create a grouped index for a list of items. if the keys are unique, use toMap instead!
	// example: val groups = levels.groupBy [ difficulty ] // creates a map with per difficulty level a list of levels 
	def static <K, V> Map<K, List<V>> groupBy(Iterable<V> list, (V)=>K indexFn) {
		val map = new HashMap<K, List<V>>
		list.forEach [
			val index = indexFn.apply(it)
			if(map.containsKey(index)) {
				val values = map.get(index)
				values.add(it)
			} else {
				val values = newLinkedList(it)
				map.put(index, values)
			}
		]
		map
	}
	
	// REDUCTION //////////////////////////////////////////////////////////////
	
	// count for each value how often it occurs
	// example: #[1, 1, 3, 2].count == #[1->2, 3->1, 2->1]
	def static <V> Map<V, Integer> count(Iterable<V> values) {
		values.countBy [ it ]
	}

	// count the occurance of values, but you can tell how to identify doubles using the index/identity function
	// example: #[user1, user2, user3].countBy [ userId ]
	def static <K, V> Map<V, Integer> countBy(Iterable<V> values, (V)=>K indexFn) {
		values
			.groupBy(indexFn)
			.toPairs
			.map [ value.head -> value.size ]
			.toMap
	}
	
	// alias for toMap for indexing a list of values	
	def static <K, V> Map<K, V> index(Iterable<V> iterable, (V)=>K indexFn) {
		iterable.toMap[ indexFn.apply(it) ]
	}
	
	def static <T extends Number> sum(Iterable<T> values) {
		var double total = 0 
		for(T value : values) { total = total + value.doubleValue }
		total
	}

	def static <T extends Number> average(Iterable<T> values) {
		values.sum / values.length
	}
	
	// IN CHECKS //////////////////////////////////////////////////////////////

	// check if an object is one of the following
	// example 12.in(#[3, 4, 12, 6]) == true
	def static <T> boolean in(T instance, List<T> objects) {
		if(instance.defined && objects.defined) objects.contains(instance) else false
	}

	// check if an object is one of the following
	// example: 12.in(3, 4, 12, 6) == true
	def static <T> boolean in(T instance, Object... objects) {
		if(instance.defined && objects.defined) objects.contains(instance) else false
	}
	
	// OPERATORS //////////////////////////////////////////////////////////////
	
	def static <T> Iterable<T> operator_doubleGreaterThan(Iterable<T> iterable, (T)=>void fn) {
		iterable.forEach(fn)
		iterable
	}

	def static <T> List<T> operator_doubleLessThan(List<T> list, T value) {
		if(list instanceof ImmutableList<?>) {
			list.addSafe(value)
		} else {
			list.add(value)
			list
		}
	}
			
}