package nl.kii.util;

import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Functions.Function2;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;

@SuppressWarnings("all")
public class FunctionExtensions {
  /**
   * Negate the result of a function
   */
  public static Function0<Boolean> operator_not(final Function0<Boolean> fn) {
    final Function0<Boolean> _function = new Function0<Boolean>() {
      public Boolean apply() {
        Boolean _apply = fn.apply();
        return Boolean.valueOf((!(_apply).booleanValue()));
      }
    };
    return _function;
  }
  
  /**
   * Negate the result of a function
   */
  public static <P1 extends Object> Function1<? super P1, Boolean> operator_not(final Function1<? super P1, Boolean> fn) {
    final Function1<P1, Boolean> _function = new Function1<P1, Boolean>() {
      public Boolean apply(final P1 it) {
        Boolean _apply = fn.apply(it);
        return Boolean.valueOf((!(_apply).booleanValue()));
      }
    };
    return _function;
  }
  
  /**
   * Negate the result of a function
   */
  public static <P1 extends Object, P2 extends Object> Function2<? super P1, ? super P2, Boolean> operator_not(final Function2<? super P1, ? super P2, Boolean> fn) {
    final Function2<P1, P2, Boolean> _function = new Function2<P1, P2, Boolean>() {
      public Boolean apply(final P1 a, final P2 b) {
        Boolean _apply = fn.apply(a, b);
        return Boolean.valueOf((!(_apply).booleanValue()));
      }
    };
    return _function;
  }
  
  /**
   * Let the << operator also work on Procedure1.apply
   */
  public static <Param extends Object> void operator_doubleLessThan(final Procedure1<Param> fn, final Param p) {
    fn.apply(p);
  }
  
  /**
   * Let the << operator also work on Function1.apply
   */
  public static <Param extends Object, T extends Object> T operator_doubleLessThan(final Function1<Param, T> fn, final Param p) {
    return fn.apply(p);
  }
  
  /**
   * Let the >> operator also work on Procedure1.apply
   */
  public static <Param extends Object> void operator_doubleGreaterThan(final Param p, final Procedure1<? super Param> fn) {
    fn.apply(p);
  }
  
  /**
   * Let the >> operator also work on Function1.apply
   */
  public static <Param extends Object, T extends Object> T operator_doubleGreaterThan(final Param p, final Function1<? super Param, ? extends T> fn) {
    return fn.apply(p);
  }
  
  /**
   * Let the _ operator also work on Procedure1.apply
   */
  public static <Param extends Object> void _(final Procedure1<Param> fn, final Param p) {
    fn.apply(p);
  }
  
  /**
   * Let the _ operator also work on Function1.apply
   */
  public static <Param extends Object, T extends Object> T _(final Function1<Param, T> fn, final Param p) {
    return fn.apply(p);
  }
  
  /**
   * Let the _ operator also work on Procedure1.apply
   */
  public static <Param extends Object> void _(final Param p, final Procedure1<? super Param> fn) {
    fn.apply(p);
  }
  
  /**
   * Let the _ operator also work on Function1.apply
   */
  public static <Param extends Object, T extends Object> T _(final Param p, final Function1<? super Param, ? extends T> fn) {
    return fn.apply(p);
  }
}
