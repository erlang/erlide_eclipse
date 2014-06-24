package nl.kii.util;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import nl.kii.util.Err;
import nl.kii.util.None;
import nl.kii.util.Opt;
import nl.kii.util.OptExtensions;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IteratorExtensions;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;

@SuppressWarnings("all")
public class IterableExtensions {
  public static <T extends Object> List<T> list(final Class<T> cls) {
    return CollectionLiterals.<T>newImmutableList();
  }
  
  public static <T extends Object> List<T> list(final T... objects) {
    return CollectionLiterals.<T>newImmutableList(objects);
  }
  
  /**
   * Always returns an immutable list, even if a null result is passed. handy when chaining, eliminates null checks
   * <pre>example: getUsers.filter[age>20].list</pre>
   */
  public static <T extends Object> List<T> toList(final Iterable<T> iterable) {
    List<T> _xifexpression = null;
    boolean _defined = OptExtensions.<Object>defined(iterable);
    boolean _not = (!_defined);
    if (_not) {
      _xifexpression = CollectionLiterals.<T>newImmutableList();
    } else {
      Iterator<T> _iterator = iterable.iterator();
      List<T> _list = IteratorExtensions.<T>toList(_iterator);
      _xifexpression = ImmutableList.<T>copyOf(_list);
    }
    return _xifexpression;
  }
  
  /**
   * Always returns an immutable set, even if a null result was passed. handy when chaining, eliminates null checks.
   * note: double values will be removed!
   */
  public static <T extends Object> Set<T> toSet(final Iterable<T> iterable) {
    Set<T> _xifexpression = null;
    boolean _defined = OptExtensions.<Object>defined(iterable);
    boolean _not = (!_defined);
    if (_not) {
      _xifexpression = CollectionLiterals.<T>newHashSet();
    } else {
      Set<T> _xblockexpression = null;
      {
        Iterable<T> _distinct = IterableExtensions.<T>distinct(iterable);
        final List<T> uniques = IterableExtensions.<T>toList(_distinct);
        HashSet<T> _hashSet = new HashSet<T>(uniques);
        _xblockexpression = ImmutableSet.<T>copyOf(_hashSet);
      }
      _xifexpression = _xblockexpression;
    }
    return _xifexpression;
  }
  
  public static <T extends Object> ImmutableList<T> addSafe(final Iterable<T> list, final T value) {
    ImmutableList.Builder<T> _builder = ImmutableList.<T>builder();
    ImmutableList.Builder<T> _addAll = _builder.addAll(list);
    ImmutableList.Builder<T> _add = _addAll.add(value);
    return _add.build();
  }
  
  public static <T extends Object> ImmutableSet<T> addSafe(final Set<T> set, final T value) {
    ImmutableSet.Builder<T> _builder = ImmutableSet.<T>builder();
    ImmutableSet.Builder<T> _addAll = _builder.addAll(set);
    ImmutableSet.Builder<T> _add = _addAll.add(value);
    return _add.build();
  }
  
  public static <T extends Object> Iterable<T> each(final Iterable<T> iterable, final Procedure1<? super T> fn) {
    Iterable<T> _xblockexpression = null;
    {
      org.eclipse.xtext.xbase.lib.IterableExtensions.<T>forEach(iterable, fn);
      _xblockexpression = iterable;
    }
    return _xblockexpression;
  }
  
  /**
   * Triggers the passed or function for each none in the list,
   * handy for tracking empty results, for example:
   * <pre>usersIds.map [ get user ].onNone [ println('could not find user') ].filterNone</pre>
   */
  public static <T extends Object> Iterable<? extends Opt<T>> onNone(final Iterable<? extends Opt<T>> iterable, final Procedure1<? super None<T>> noneHandler) {
    Iterable<? extends Opt<T>> _xblockexpression = null;
    {
      final Function1<Opt<T>, Boolean> _function = new Function1<Opt<T>, Boolean>() {
        public Boolean apply(final Opt<T> it) {
          return Boolean.valueOf(it.hasNone());
        }
      };
      Iterable<? extends Opt<T>> _filter = org.eclipse.xtext.xbase.lib.IterableExtensions.filter(iterable, _function);
      final Procedure1<Opt<T>> _function_1 = new Procedure1<Opt<T>>() {
        public void apply(final Opt<T> it) {
          noneHandler.apply(((None<T>) it));
        }
      };
      IterableExtensions.each(_filter, _function_1);
      _xblockexpression = iterable;
    }
    return _xblockexpression;
  }
  
  /**
   * Triggers the passed or function for each error in the list,
   * handy for tracking errors for example:
   * <pre>usersIds.attemptMap [ get user ].onError [ error handling ].filterEmpty</pre>
   */
  public static <T extends Object> Iterable<? extends Opt<T>> onError(final Iterable<? extends Opt<T>> iterable, final Procedure1<? super Err<T>> errorHandler) {
    Iterable<? extends Opt<T>> _xblockexpression = null;
    {
      final Function1<Opt<T>, Boolean> _function = new Function1<Opt<T>, Boolean>() {
        public Boolean apply(final Opt<T> it) {
          return Boolean.valueOf(it.hasError());
        }
      };
      Iterable<? extends Opt<T>> _filter = org.eclipse.xtext.xbase.lib.IterableExtensions.filter(iterable, _function);
      final Procedure1<Opt<T>> _function_1 = new Procedure1<Opt<T>>() {
        public void apply(final Opt<T> it) {
          errorHandler.apply(((Err<T>) it));
        }
      };
      IterableExtensions.each(_filter, _function_1);
      _xblockexpression = iterable;
    }
    return _xblockexpression;
  }
  
  /**
   * Convert a list of options into actual values, filtering out the none and error values.
   * Like filterNull, but then for a list of Options
   */
  public static <T extends Object> Iterable<T> filterEmpty(final Iterable<? extends Opt<T>> iterable) {
    final Function1<Opt<T>, T> _function = new Function1<Opt<T>, T>() {
      public T apply(final Opt<T> it) {
        return OptExtensions.<T>orNull(it);
      }
    };
    Iterable<T> _map = org.eclipse.xtext.xbase.lib.IterableExtensions.map(iterable, _function);
    return org.eclipse.xtext.xbase.lib.IterableExtensions.<T>filterNull(_map);
  }
  
  public static <T extends Object> Iterable<? extends Opt<T>> filterError(final Iterable<? extends Opt<T>> iterable) {
    final Function1<Opt<T>, Boolean> _function = new Function1<Opt<T>, Boolean>() {
      public Boolean apply(final Opt<T> it) {
        boolean _hasError = it.hasError();
        return Boolean.valueOf((!_hasError));
      }
    };
    return org.eclipse.xtext.xbase.lib.IterableExtensions.filter(iterable, _function);
  }
  
  /**
   * Remove all double values in a list, turning it into a list of unique values
   */
  public static <T extends Object> Iterable<T> distinct(final Iterable<T> values) {
    final Function1<T, T> _function = new Function1<T, T>() {
      public T apply(final T it) {
        return it;
      }
    };
    Map<T, List<T>> _groupBy = IterableExtensions.<T, T>groupBy(values, _function);
    Iterable<Pair<T, List<T>>> _pairs = IterableExtensions.<T, List<T>>toPairs(_groupBy);
    final Function1<Pair<T, List<T>>, T> _function_1 = new Function1<Pair<T, List<T>>, T>() {
      public T apply(final Pair<T, List<T>> it) {
        List<T> _value = it.getValue();
        return org.eclipse.xtext.xbase.lib.IterableExtensions.<T>head(_value);
      }
    };
    return org.eclipse.xtext.xbase.lib.IterableExtensions.<Pair<T, List<T>>, T>map(_pairs, _function_1);
  }
  
  public static <T extends Object, R extends Object> Iterable<Opt<R>> mapOpt(final Iterable<Opt<T>> iterable, final Function1<? super T, ? extends R> fn) {
    final Function1<Opt<T>, Opt<R>> _function = new Function1<Opt<T>, Opt<R>>() {
      public Opt<R> apply(final Opt<T> it) {
        return OptExtensions.<R, T>mapOpt(it, fn);
      }
    };
    return org.eclipse.xtext.xbase.lib.IterableExtensions.<Opt<T>, Opt<R>>map(iterable, _function);
  }
  
  public static <T extends Object, R extends Object> Iterable<Opt<R>> attemptMap(final Iterable<T> iterable, final Function1<? super T, ? extends R> fn) {
    final Function1<T, Opt<R>> _function = new Function1<T, Opt<R>>() {
      public Opt<R> apply(final T it) {
        Opt<R> _xblockexpression = null;
        {
          final T o = it;
          final Function1<Object, R> _function = new Function1<Object, R>() {
            public R apply(final Object it) {
              return fn.apply(o);
            }
          };
          _xblockexpression = OptExtensions.<R>attempt(_function);
        }
        return _xblockexpression;
      }
    };
    return org.eclipse.xtext.xbase.lib.IterableExtensions.<T, Opt<R>>map(iterable, _function);
  }
  
  public static <K extends Object, V extends Object> Iterable<Pair<K, V>> toPairs(final Map<K, V> map) {
    Set<Map.Entry<K, V>> _entrySet = map.entrySet();
    final Function1<Map.Entry<K, V>, Pair<K, V>> _function = new Function1<Map.Entry<K, V>, Pair<K, V>>() {
      public Pair<K, V> apply(final Map.Entry<K, V> it) {
        K _key = it.getKey();
        V _value = it.getValue();
        return Pair.<K, V>of(_key, _value);
      }
    };
    return org.eclipse.xtext.xbase.lib.IterableExtensions.<Map.Entry<K, V>, Pair<K, V>>map(_entrySet, _function);
  }
  
  public static <K extends Object, V extends Object> Map<K, V> toMap(final Iterable<Pair<K, V>> pairs) {
    HashMap<K, V> _xblockexpression = null;
    {
      final HashMap<K, V> map = CollectionLiterals.<K, V>newHashMap();
      boolean _defined = OptExtensions.<Object>defined(pairs);
      if (_defined) {
        final Procedure1<Pair<K, V>> _function = new Procedure1<Pair<K, V>>() {
          public void apply(final Pair<K, V> it) {
            K _key = it.getKey();
            V _value = it.getValue();
            map.put(_key, _value);
          }
        };
        org.eclipse.xtext.xbase.lib.IterableExtensions.<Pair<K, V>>forEach(pairs, _function);
      }
      _xblockexpression = map;
    }
    return _xblockexpression;
  }
  
  public static <K extends Object, V extends Object> Map<K, List<V>> groupBy(final Iterable<V> list, final Function1<? super V, ? extends K> indexFn) {
    HashMap<K, List<V>> _xblockexpression = null;
    {
      final HashMap<K, List<V>> map = new HashMap<K, List<V>>();
      final Procedure1<V> _function = new Procedure1<V>() {
        public void apply(final V it) {
          final K index = indexFn.apply(it);
          boolean _containsKey = map.containsKey(index);
          if (_containsKey) {
            final List<V> values = map.get(index);
            values.add(it);
          } else {
            final LinkedList<V> values_1 = CollectionLiterals.<V>newLinkedList(it);
            map.put(index, values_1);
          }
        }
      };
      org.eclipse.xtext.xbase.lib.IterableExtensions.<V>forEach(list, _function);
      _xblockexpression = map;
    }
    return _xblockexpression;
  }
  
  public static <V extends Object> Map<V, Integer> count(final Iterable<V> values) {
    final Function1<V, V> _function = new Function1<V, V>() {
      public V apply(final V it) {
        return it;
      }
    };
    return IterableExtensions.<V, V>countBy(values, _function);
  }
  
  public static <K extends Object, V extends Object> Map<V, Integer> countBy(final Iterable<V> values, final Function1<? super V, ? extends K> indexFn) {
    Map<K, List<V>> _groupBy = IterableExtensions.<K, V>groupBy(values, indexFn);
    Iterable<Pair<K, List<V>>> _pairs = IterableExtensions.<K, List<V>>toPairs(_groupBy);
    final Function1<Pair<K, List<V>>, Pair<V, Integer>> _function = new Function1<Pair<K, List<V>>, Pair<V, Integer>>() {
      public Pair<V, Integer> apply(final Pair<K, List<V>> it) {
        List<V> _value = it.getValue();
        V _head = org.eclipse.xtext.xbase.lib.IterableExtensions.<V>head(_value);
        List<V> _value_1 = it.getValue();
        int _size = _value_1.size();
        return Pair.<V, Integer>of(_head, Integer.valueOf(_size));
      }
    };
    Iterable<Pair<V, Integer>> _map = org.eclipse.xtext.xbase.lib.IterableExtensions.<Pair<K, List<V>>, Pair<V, Integer>>map(_pairs, _function);
    return IterableExtensions.<V, Integer>toMap(_map);
  }
  
  public static <K extends Object, V extends Object> Map<K, V> index(final Iterable<V> iterable, final Function1<? super V, ? extends K> indexFn) {
    final Function1<V, K> _function = new Function1<V, K>() {
      public K apply(final V it) {
        return indexFn.apply(it);
      }
    };
    return org.eclipse.xtext.xbase.lib.IterableExtensions.<K, V>toMap(iterable, _function);
  }
  
  public static <T extends Number> double sum(final Iterable<T> values) {
    double _xblockexpression = (double) 0;
    {
      double total = 0;
      for (final T value : values) {
        double _doubleValue = value.doubleValue();
        double _plus = (total + _doubleValue);
        total = _plus;
      }
      _xblockexpression = total;
    }
    return _xblockexpression;
  }
  
  public static <T extends Number> double average(final Iterable<T> values) {
    double _sum = IterableExtensions.<T>sum(values);
    int _length = ((Object[])Conversions.unwrapArray(values, Object.class)).length;
    return (_sum / _length);
  }
  
  public static <T extends Object> boolean in(final T instance, final List<T> objects) {
    boolean _xifexpression = false;
    boolean _and = false;
    boolean _defined = OptExtensions.<Object>defined(instance);
    if (!_defined) {
      _and = false;
    } else {
      boolean _defined_1 = OptExtensions.<Object>defined(objects);
      _and = _defined_1;
    }
    if (_and) {
      _xifexpression = objects.contains(instance);
    } else {
      _xifexpression = false;
    }
    return _xifexpression;
  }
  
  public static <T extends Object> boolean in(final T instance, final Object... objects) {
    boolean _xifexpression = false;
    boolean _and = false;
    boolean _defined = OptExtensions.<Object>defined(instance);
    if (!_defined) {
      _and = false;
    } else {
      boolean _defined_1 = OptExtensions.<Object>defined(objects);
      _and = _defined_1;
    }
    if (_and) {
      _xifexpression = ((List<Object>)Conversions.doWrapArray(objects)).contains(instance);
    } else {
      _xifexpression = false;
    }
    return _xifexpression;
  }
  
  public static <T extends Object> Iterable<T> operator_doubleGreaterThan(final Iterable<T> iterable, final Procedure1<? super T> fn) {
    Iterable<T> _xblockexpression = null;
    {
      org.eclipse.xtext.xbase.lib.IterableExtensions.<T>forEach(iterable, fn);
      _xblockexpression = iterable;
    }
    return _xblockexpression;
  }
  
  public static <T extends Object> List<T> operator_doubleLessThan(final List<T> list, final T value) {
    List<T> _xifexpression = null;
    if ((list instanceof ImmutableList<?>)) {
      _xifexpression = IterableExtensions.<T>addSafe(list, value);
    } else {
      List<T> _xblockexpression = null;
      {
        list.add(value);
        _xblockexpression = list;
      }
      _xifexpression = _xblockexpression;
    }
    return _xifexpression;
  }
}
