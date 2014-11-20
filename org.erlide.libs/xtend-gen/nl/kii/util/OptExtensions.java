package nl.kii.util;

import com.google.common.base.Objects;
import nl.kii.util.Err;
import nl.kii.util.None;
import nl.kii.util.NoneException;
import nl.kii.util.Opt;
import nl.kii.util.Some;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;

@SuppressWarnings("all")
public class OptExtensions {
  /**
   * Checks if an object is defined, meaning here that it is not empty or faulty
   */
  public static <T extends Object> boolean defined(final Object o) {
    boolean _switchResult = false;
    boolean _matched = false;
    if (!_matched) {
      if (Objects.equal(o, null)) {
        _matched=true;
        _switchResult = false;
      }
    }
    if (!_matched) {
      if (o instanceof None) {
        _matched=true;
        _switchResult = false;
      }
    }
    if (!_matched) {
      if (o instanceof Err) {
        _matched=true;
        _switchResult = false;
      }
    }
    if (!_matched) {
      _switchResult = true;
    }
    return _switchResult;
  }
  
  /**
   * Only perform the function for a given condition. Returns an optional result.
   * <pre>val Opt<User> user = ifTrue(isMale) [ getUser ]</pre>
   */
  public static <T extends Object, I extends Object> Opt<T> ifTrue(final boolean condition, final Function1<? super Object, ? extends T> fn) {
    Opt<T> _xifexpression = null;
    if (condition) {
      T _apply = fn.apply(null);
      _xifexpression = OptExtensions.<T>option(_apply);
    } else {
      _xifexpression = OptExtensions.<T>none();
    }
    return _xifexpression;
  }
  
  /**
   * Only perform the function if something was set. Returns an optional result.
   * <pre>val Opt<User> user = ifSome(userId) [ getUser(userId) ]</pre>
   */
  public static <T extends Object, I extends Object> Opt<T> ifSome(final Opt<I> o, final Function1<? super I, ? extends T> fn) {
    try {
      Opt<T> _xifexpression = null;
      boolean _defined = OptExtensions.<Object>defined(o);
      if (_defined) {
        I _value = o.value();
        T _apply = fn.apply(_value);
        _xifexpression = OptExtensions.<T>option(_apply);
      } else {
        _xifexpression = OptExtensions.<T>none();
      }
      return _xifexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  /**
   * Only perform the function if something was set. Returns the result of the function for chaining
   */
  public static <T extends Object, I extends Object> Opt<I> ifSome(final Opt<I> o, final Procedure1<? super I> fn) {
    try {
      Opt<I> _xblockexpression = null;
      {
        boolean _defined = OptExtensions.<Object>defined(o);
        if (_defined) {
          I _value = o.value();
          fn.apply(_value);
        }
        _xblockexpression = o;
      }
      return _xblockexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  /**
   * Only perform the function if o is a None. Returns an optional result.
   * <pre>val Opt<User> user = ifSome(userId) [ getUser(it) ]</pre>
   */
  public static <T extends Object, I extends Object> Opt<T> ifNone(final Opt<I> o, final Function1<? super I, ? extends T> fn) {
    try {
      Opt<T> _xifexpression = null;
      boolean _hasNone = o.hasNone();
      if (_hasNone) {
        I _value = o.value();
        T _apply = fn.apply(_value);
        _xifexpression = OptExtensions.<T>option(_apply);
      } else {
        _xifexpression = OptExtensions.<T>none();
      }
      return _xifexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  /**
   * Only perform the function if o is a None. Returns the result of the function for chaining
   */
  public static <T extends Object, I extends Object> Opt<I> ifNone(final Opt<I> o, final Procedure1<? super Object> fn) {
    Opt<I> _xblockexpression = null;
    {
      boolean _hasNone = o.hasNone();
      if (_hasNone) {
        fn.apply(null);
      }
      _xblockexpression = o;
    }
    return _xblockexpression;
  }
  
  /**
   * Only perform the function if the passed argument was empty,
   * meaning null or empty or an error. Returns an optional result.
   * <pre>val Opt<User> user = ifEmpty(userId) [ getDefaultUser ]</pre>
   */
  public static <T extends Object, I extends Object> Opt<T> ifEmpty(final Opt<I> o, final Function1<? super I, ? extends T> fn) {
    try {
      Opt<T> _xifexpression = null;
      boolean _defined = OptExtensions.<Object>defined(o);
      boolean _not = (!_defined);
      if (_not) {
        I _value = o.value();
        T _apply = fn.apply(_value);
        _xifexpression = OptExtensions.<T>option(_apply);
      } else {
        _xifexpression = OptExtensions.<T>none();
      }
      return _xifexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  /**
   * Only perform the function if the passed argument was an error result.
   * Returns an optional result.
   * <pre>
   * val Opt<User> user = getUserOpt(userId)
   * val Opt<String> warning = ifError(user) [ 'something went wrong!' ]
   * </pre>
   */
  public static <T extends Object, I extends Object> Opt<T> ifErr(final Opt<I> o, final Function1<? super I, ? extends T> fn) {
    try {
      Opt<T> _xifexpression = null;
      boolean _hasError = o.hasError();
      if (_hasError) {
        I _value = o.value();
        T _apply = fn.apply(_value);
        _xifexpression = OptExtensions.<T>option(_apply);
      } else {
        _xifexpression = OptExtensions.<T>none();
      }
      return _xifexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  /**
   * Only perform the function if o is an Err. Returns the result of the function for chaining
   */
  public static <T extends Object, I extends Object> Opt<I> ifErr(final Opt<I> o, final Procedure1<? super Throwable> fn) {
    Opt<I> _xblockexpression = null;
    {
      boolean _matched = false;
      if (!_matched) {
        if (o instanceof Err) {
          _matched=true;
          Throwable _exception = ((Err<I>)o).getException();
          fn.apply(_exception);
        }
      }
      _xblockexpression = o;
    }
    return _xblockexpression;
  }
  
  public static <T extends Object> T operator_elvis(final Opt<T> option, final T fallback) {
    try {
      T _xifexpression = null;
      boolean _hasSome = option.hasSome();
      if (_hasSome) {
        _xifexpression = option.value();
      } else {
        _xifexpression = fallback;
      }
      return _xifexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public static <T extends Object> T operator_elvis(final Opt<T> option, final Function1<? super Void, ? extends T> fallback) {
    try {
      T _xifexpression = null;
      boolean _hasSome = option.hasSome();
      if (_hasSome) {
        _xifexpression = option.value();
      } else {
        _xifexpression = fallback.apply(null);
      }
      return _xifexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  /**
   * Create an option from an object. It detect if it is a None or a Some.
   * <pre>api.getUser(userId).option // if getUser returns null, it will be None, otherwise Some<User></pre>
   */
  public static <T extends Object> Opt<T> option(final T value) {
    Opt<T> _xifexpression = null;
    boolean _defined = OptExtensions.<Object>defined(value);
    if (_defined) {
      _xifexpression = OptExtensions.<T>some(value);
    } else {
      Opt<T> _xifexpression_1 = null;
      if ((value instanceof Err<?>)) {
        Throwable _exception = ((Err<?>)value).getException();
        _xifexpression_1 = OptExtensions.<T>err(_exception);
      } else {
        _xifexpression_1 = OptExtensions.<T>none();
      }
      _xifexpression = _xifexpression_1;
    }
    return _xifexpression;
  }
  
  public static <T extends Object> Some<T> some(final T value) {
    return new Some<T>(value);
  }
  
  public static <T extends Object> None<T> none() {
    return new None<T>();
  }
  
  public static <T extends Object> Err<T> err(final Throwable t) {
    return new Err<T>(t);
  }
  
  public static <T extends Object> Err<T> err() {
    return new Err<T>();
  }
  
  /**
   * wrap a call as an option (exception or null generates none)<p>
   * example: val userOption = attempt [ api.getUser(userId) ] // if API throws exception, return None
   */
  public static <T extends Object> Opt<T> attempt(final Function1<? super Object, ? extends T> fn) {
    Opt<T> _xtrycatchfinallyexpression = null;
    try {
      T _apply = fn.apply(null);
      _xtrycatchfinallyexpression = OptExtensions.<T>option(_apply);
    } catch (final Throwable _t) {
      if (_t instanceof Exception) {
        final Exception e = (Exception)_t;
        _xtrycatchfinallyexpression = OptExtensions.<T>err(e);
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
    return _xtrycatchfinallyexpression;
  }
  
  /**
   * wrap a call as an option (exception or null generates none)<p>
   * example: val userOption = attempt [ api.getUser(userId) ] // if API throws exception, return None
   */
  public static <P extends Object, T extends Object> Opt<T> attempt(final P parm, final Function1<? super P, ? extends T> fn) {
    Opt<T> _xtrycatchfinallyexpression = null;
    try {
      T _apply = fn.apply(parm);
      _xtrycatchfinallyexpression = OptExtensions.<T>option(_apply);
    } catch (final Throwable _t) {
      if (_t instanceof Exception) {
        final Exception e = (Exception)_t;
        _xtrycatchfinallyexpression = OptExtensions.<T>err(e);
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
    return _xtrycatchfinallyexpression;
  }
  
  /**
   * Same as => but with optional execution and option result<p>
   * example: normally you do: user => [ name = 'john' ]<p>
   * but what if user is of type Option<User><p>
   * then you can do: user.attempt [ name = 'john' ]<br>
   * the assignment will only complete if there was a user
   */
  public static <T extends Object, O extends Object> Opt<O> attempt(final Opt<O> o, final Function1<? super O, ? extends T> fn) {
    try {
      Opt<O> _xblockexpression = null;
      {
        boolean _defined = OptExtensions.<Object>defined(o);
        if (_defined) {
          O _value = o.value();
          fn.apply(_value);
        }
        _xblockexpression = o;
      }
      return _xblockexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
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
  public static <T extends Object, O extends Object> Opt<O> attempt(final Opt<O> o, final Procedure1<? super O> fn) {
    try {
      Opt<O> _xblockexpression = null;
      {
        boolean _defined = OptExtensions.<Object>defined(o);
        if (_defined) {
          O _value = o.value();
          fn.apply(_value);
        }
        _xblockexpression = o;
      }
      return _xblockexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  /**
   * Transform an option into a new option using a function.
   * The function allows you to transform the value of the passed option,
   * saving you the need to unwrap it yourself
   */
  public static <T extends Object, I extends Object> Opt<T> map(final Opt<I> o, final Function1<? super I, ? extends T> fn) {
    try {
      Opt<T> _xifexpression = null;
      boolean _defined = OptExtensions.<Object>defined(o);
      if (_defined) {
        I _value = o.value();
        T _apply = fn.apply(_value);
        _xifexpression = OptExtensions.<T>option(_apply);
      } else {
        _xifexpression = OptExtensions.<T>none();
      }
      return _xifexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  /**
   * Flatten a double wrapped optional back to a single optional
   */
  public static <T extends Object> Opt<T> flatten(final Opt<Opt<T>> option) {
    Opt<T> _switchResult = null;
    boolean _matched = false;
    if (!_matched) {
      if (option instanceof Some) {
        _matched=true;
        _switchResult = ((Some<Opt<T>>)option).value();
      }
    }
    if (!_matched) {
      if (option instanceof None) {
        _matched=true;
        _switchResult = OptExtensions.<T>none();
      }
    }
    if (!_matched) {
      if (option instanceof Err) {
        _matched=true;
        Throwable _exception = ((Err<Opt<T>>)option).getException();
        _switchResult = OptExtensions.<T>err(_exception);
      }
    }
    return _switchResult;
  }
  
  /**
   * Provide a fallback value if o is undefined
   *  <pre>val user = foundUser.or(defaultUser)</pre>
   */
  public static <T extends Object> T or(final T o, final T fallback) {
    T _xifexpression = null;
    boolean _defined = OptExtensions.<Object>defined(o);
    if (_defined) {
      _xifexpression = o;
    } else {
      _xifexpression = fallback;
    }
    return _xifexpression;
  }
  
  /**
   * provide a fallback value if o is undefined
   * <pre>val user = api.getUser(12).or(defaultUser) // getUser returns an Option<User></pre>
   */
  public static <T extends Object> T or(final Opt<T> o, final T fallback) {
    try {
      T _xifexpression = null;
      boolean _defined = OptExtensions.<Object>defined(o);
      if (_defined) {
        _xifexpression = o.value();
      } else {
        _xifexpression = fallback;
      }
      return _xifexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public static <T extends Object> T or(final T o, final Function1<? super Object, ? extends T> fallbackFn) {
    T _xifexpression = null;
    boolean _defined = OptExtensions.<Object>defined(o);
    if (_defined) {
      _xifexpression = o;
    } else {
      _xifexpression = fallbackFn.apply(null);
    }
    return _xifexpression;
  }
  
  public static <T extends Object> T or(final Opt<T> o, final Function1<? super Object, ? extends T> fallbackFn) {
    try {
      T _xifexpression = null;
      boolean _defined = OptExtensions.<Object>defined(o);
      if (_defined) {
        _xifexpression = o.value();
      } else {
        _xifexpression = fallbackFn.apply(null);
      }
      return _xifexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public static <T extends Object> T orNull(final T o) {
    T _xifexpression = null;
    boolean _defined = OptExtensions.<Object>defined(o);
    if (_defined) {
      _xifexpression = o;
    } else {
      _xifexpression = null;
    }
    return _xifexpression;
  }
  
  public static <T extends Object> T orNull(final Opt<T> o) {
    try {
      T _xifexpression = null;
      boolean _defined = OptExtensions.<Object>defined(o);
      if (_defined) {
        _xifexpression = o.value();
      } else {
        _xifexpression = null;
      }
      return _xifexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public static <T extends Object> T orThrow(final Opt<T> o) {
    try {
      T _switchResult = null;
      boolean _matched = false;
      if (!_matched) {
        if (o instanceof Err) {
          _matched=true;
          throw ((Err<T>)o).getException();
        }
      }
      if (!_matched) {
        if (o instanceof None) {
          _matched=true;
          throw new NoneException();
        }
      }
      if (!_matched) {
        if (o instanceof Some) {
          _matched=true;
          _switchResult = ((Some<T>)o).value();
        }
      }
      return _switchResult;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public static <T extends Object> T orThrow(final Opt<T> o, final String s) {
    try {
      T _xifexpression = null;
      boolean _defined = OptExtensions.<Object>defined(o);
      if (_defined) {
        _xifexpression = o.value();
      } else {
        throw new NoneException(s);
      }
      return _xifexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public static <T extends Object> T orThrow(final Opt<T> o, final Function1<? super Object, ? extends Throwable> exceptionFn) {
    try {
      T _xifexpression = null;
      boolean _defined = OptExtensions.<Object>defined(o);
      if (_defined) {
        _xifexpression = o.value();
      } else {
        throw exceptionFn.apply(null);
      }
      return _xifexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}
