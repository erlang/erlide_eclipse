package nl.kii.util;

import java.io.Closeable;
import nl.kii.util.Opt;
import nl.kii.util.OptExtensions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;

@SuppressWarnings("all")
public class CloseableExtensions {
  /**
   * Perform an operation on a closable, and close it when finished
   */
  public static <I extends Closeable> void using(final I closable, final Procedure1<? super I> fn) {
    try {
      try {
        fn.apply(closable);
      } finally {
        closable.close();
      }
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  /**
   * Perform an operation on a closable, and close it when finished
   */
  public static <I extends Closeable, T extends Object> T using(final I closable, final Function1<? super I, ? extends T> fn) {
    try {
      T _xtrycatchfinallyexpression = null;
      try {
        _xtrycatchfinallyexpression = fn.apply(closable);
      } finally {
        closable.close();
      }
      return _xtrycatchfinallyexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public static <I extends Closeable, T extends Object> Opt<T> attemptUsing(final I closable, final Function1<? super I, ? extends T> fn) {
    Opt<T> _xtrycatchfinallyexpression = null;
    try {
      T _using = CloseableExtensions.<I, T>using(closable, fn);
      _xtrycatchfinallyexpression = OptExtensions.<T>option(_using);
    } catch (final Throwable _t) {
      if (_t instanceof Throwable) {
        final Throwable e = (Throwable)_t;
        _xtrycatchfinallyexpression = OptExtensions.<T>error(e);
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
    return _xtrycatchfinallyexpression;
  }
}
