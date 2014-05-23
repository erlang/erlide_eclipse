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
  public static <T extends Object, I extends Object> Opt<T> ifError(final Opt<I> o, final Function1<? super I, ? extends T> fn) {
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
   * Create an option from an object. It detect if it is a None or a Some.
   * <pre>api.getUser(userId).option // if getUser returns null, it will be None, otherwise Some<User></pre>
   */
  public static <T extends Object> Opt<T> option(final T value) {
    Opt<T> _xifexpression = null;
    boolean _defined = OptExtensions.<Object>defined(value);
    if (_defined) {
      _xifexpression = OptExtensions.<T>some(value);
    } else {
      _xifexpression = OptExtensions.<T>none();
    }
    return _xifexpression;
  }
  
  public static <T extends Object> Some<T> some(final T value) {
    return new Some<T>(value);
  }
  
  public static <T extends Object> None<T> none() {
    return new None<T>();
  }
  
  public static <T extends Object> Err<T> error(final Throwable t) {
    return new Err<T>(t);
  }
  
  public static <T extends Object> Err<T> error() {
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
        _xtrycatchfinallyexpression = OptExtensions.<T>error(e);
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
  
  public static <I extends Object, T extends Object> I apply(final I input, final Procedure1<? super I> fn) {
    I _xblockexpression = null;
    {
      fn.apply(input);
      _xblockexpression = input;
    }
    return _xblockexpression;
  }
  
  /**
   * Transform an option into a new option using a function.
   * The function allows you to transform the value of the passed option,
   * saving you the need to unwrap it yourself
   */
  public static <T extends Object, I extends Object> Opt<T> mapOpt(final Opt<I> o, final Function1<? super I, ? extends T> fn) {
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
  
  public static <T extends Object> T orThrow(final T o, final Function1<? super Object, ? extends Throwable> exceptionFn) {
    try {
      T _xifexpression = null;
      boolean _defined = OptExtensions.<Object>defined(o);
      if (_defined) {
        _xifexpression = o;
      } else {
        throw exceptionFn.apply(null);
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
