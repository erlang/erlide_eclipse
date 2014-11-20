package nl.kii.util;

import com.google.common.base.Objects;
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
import org.eclipse.xtext.xbase.lib.Functions.Function2;
import org.eclipse.xtext.xbase.lib.IteratorExtensions;
import org.eclipse.xtext.xbase.lib.ListExtensions;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure2;

@SuppressWarnings("all")
public class IterableExtensions {
  public static <T extends Object> List<T> list(final Class<T> cls) {
    return CollectionLiterals.<T>newImmutableList();
  }
  
  public static <T extends Object> List<T> list(final T... objects) {
    return CollectionLiterals.<T>newImmutableList(objects);
  }
  
  /**
   * Returns if a list is of a certain value type by looking at the first value.
   * @returns true if the list is not empty and the type matches
   * <pre>
   * #[1, 2, 3].isListOf(Integer) // true
   * #[1, 2, 3].isListOf(String) // false
   * #[].isListOf(Integer) // false
   */
  public static boolean isListOf(final List<?> list, final Class<?> type) {
    boolean _xblockexpression = false;
    {
      boolean _isEmpty = list.isEmpty();
      if (_isEmpty) {
        return false;
      }
      Object _get = list.get(0);
      Class<?> _class = _get.getClass();
      _xblockexpression = _class.isAssignableFrom(type);
    }
    return _xblockexpression;
  }
  
  /**
   * Returns if a map is of a certain key and value type by looking at the first value.
   * @returns true if the map is not empty and the types match
   * <pre>
   * #{1->'A', 5->'C'}.isMapOf(Integer, String) // true
   * #{1->'A', 5->'C'}.isMapOf(Integer, Integer) // false
   * #{}.isMapOf(Integer, String) // false
   */
  public static boolean isMapOf(final Map<?, ?> map, final Class<?> keyType, final Class<?> valueType) {
    boolean _xblockexpression = false;
    {
      boolean _isEmpty = map.isEmpty();
      if (_isEmpty) {
        return false;
      }
      Set<?> _keySet = map.keySet();
      final Object key0 = ((Object[])Conversions.unwrapArray(_keySet, Object.class))[0];
      boolean _and = false;
      Class<?> _class = key0.getClass();
      boolean _isAssignableFrom = _class.isAssignableFrom(keyType);
      if (!_isAssignableFrom) {
        _and = false;
      } else {
        Object _get = map.get(key0);
        Class<?> _class_1 = _get.getClass();
        boolean _isAssignableFrom_1 = _class_1.isAssignableFrom(valueType);
        _and = _isAssignableFrom_1;
      }
      _xblockexpression = _and;
    }
    return _xblockexpression;
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
  public static <T extends Object> Set<T> toSet(final Iterable<? extends T> iterable) {
    Set<T> _xifexpression = null;
    boolean _defined = OptExtensions.<Object>defined(iterable);
    boolean _not = (!_defined);
    if (_not) {
      _xifexpression = CollectionLiterals.<T>newHashSet();
    } else {
      Set<T> _xblockexpression = null;
      {
        List<T> _distinct = IterableExtensions.<T>distinct(iterable);
        final List<T> uniques = IterableExtensions.<T>toList(_distinct);
        HashSet<T> _hashSet = new HashSet<T>(uniques);
        _xblockexpression = ImmutableSet.<T>copyOf(_hashSet);
      }
      _xifexpression = _xblockexpression;
    }
    return _xifexpression;
  }
  
  public static <T extends Object> ImmutableList<T> concat(final Iterable<? extends T> list, final T value) {
    ImmutableList<T> _xifexpression = null;
    boolean _tripleNotEquals = (value != null);
    if (_tripleNotEquals) {
      ImmutableList.Builder<T> _builder = ImmutableList.<T>builder();
      ImmutableList.Builder<T> _addAll = _builder.addAll(list);
      ImmutableList.Builder<T> _add = _addAll.add(value);
      _xifexpression = _add.build();
    }
    return _xifexpression;
  }
  
  public static <T extends Object> ImmutableList<T> concat(final T value, final Iterable<T> list) {
    ImmutableList<T> _xifexpression = null;
    boolean _tripleNotEquals = (list != null);
    if (_tripleNotEquals) {
      ImmutableList.Builder<T> _builder = ImmutableList.<T>builder();
      ImmutableList.Builder<T> _add = _builder.add(value);
      ImmutableList.Builder<T> _addAll = _add.addAll(list);
      _xifexpression = _addAll.build();
    }
    return _xifexpression;
  }
  
  public static <T extends Object> ImmutableSet<T> concat(final Set<T> set, final T value) {
    ImmutableSet<T> _xifexpression = null;
    boolean _tripleNotEquals = (value != null);
    if (_tripleNotEquals) {
      ImmutableSet.Builder<T> _builder = ImmutableSet.<T>builder();
      ImmutableSet.Builder<T> _addAll = _builder.addAll(set);
      ImmutableSet.Builder<T> _add = _addAll.add(value);
      _xifexpression = _add.build();
    }
    return _xifexpression;
  }
  
  public static <T extends Object> ImmutableSet<T> concat(final T value, final Set<T> set) {
    ImmutableSet<T> _xifexpression = null;
    boolean _tripleNotEquals = (set != null);
    if (_tripleNotEquals) {
      ImmutableSet.Builder<T> _builder = ImmutableSet.<T>builder();
      ImmutableSet.Builder<T> _add = _builder.add(value);
      ImmutableSet.Builder<T> _addAll = _add.addAll(set);
      _xifexpression = _addAll.build();
    }
    return _xifexpression;
  }
  
  public static <T extends Object> ImmutableList<T> uncat(final List<? extends T> list, final T value) {
    ImmutableList.Builder<T> _builder = ImmutableList.<T>builder();
    final Function1<T, Boolean> _function = new Function1<T, Boolean>() {
      public Boolean apply(final T it) {
        return Boolean.valueOf((!Objects.equal(it, value)));
      }
    };
    Iterable<? extends T> _filter = org.eclipse.xtext.xbase.lib.IterableExtensions.filter(list, _function);
    ImmutableList.Builder<T> _addAll = _builder.addAll(_filter);
    return _addAll.build();
  }
  
  public static <T extends Object> ImmutableList<T> uncat(final List<? extends T> list, final List<? extends T> list2) {
    ImmutableList.Builder<T> _builder = ImmutableList.<T>builder();
    final Function1<T, Boolean> _function = new Function1<T, Boolean>() {
      public Boolean apply(final T it) {
        boolean _contains = list2.contains(it);
        return Boolean.valueOf((!_contains));
      }
    };
    Iterable<? extends T> _filter = org.eclipse.xtext.xbase.lib.IterableExtensions.filter(list, _function);
    ImmutableList.Builder<T> _addAll = _builder.addAll(_filter);
    return _addAll.build();
  }
  
  public static <T extends Object> Iterable<? extends T> effect(final Iterable<? extends T> iterable, final Procedure1<? super T> fn) {
    Iterable<? extends T> _xblockexpression = null;
    {
      org.eclipse.xtext.xbase.lib.IterableExtensions.forEach(iterable, fn);
      _xblockexpression = iterable;
    }
    return _xblockexpression;
  }
  
  public static <T extends Object> Iterable<? extends T> effect(final Iterable<? extends T> iterable, final Procedure2<? super T, ? super Integer> fn) {
    Iterable<? extends T> _xblockexpression = null;
    {
      org.eclipse.xtext.xbase.lib.IterableExtensions.forEach(iterable, fn);
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
      IterableExtensions.<Opt<T>>effect(_filter, _function_1);
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
      IterableExtensions.<Opt<T>>effect(_filter, _function_1);
      _xblockexpression = iterable;
    }
    return _xblockexpression;
  }
  
  public static <T extends Object> T last(final Iterable<? extends T> values) {
    List<? extends T> _list = IterableExtensions.toList(values);
    List<? extends T> _reverse = ListExtensions.reverse(_list);
    return org.eclipse.xtext.xbase.lib.IterableExtensions.head(_reverse);
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
  public static <T extends Object> List<T> distinct(final Iterable<? extends T> values) {
    final Function1<T, T> _function = new Function1<T, T>() {
      public T apply(final T it) {
        return it;
      }
    };
    Map<T, List<T>> _groupBy = IterableExtensions.<T, T>groupBy(values, _function);
    List<Pair<T, List<T>>> _pairs = IterableExtensions.<T, List<T>>toPairs(_groupBy);
    final Function1<Pair<T, List<T>>, T> _function_1 = new Function1<Pair<T, List<T>>, T>() {
      public T apply(final Pair<T, List<T>> it) {
        List<T> _value = it.getValue();
        return org.eclipse.xtext.xbase.lib.IterableExtensions.<T>head(_value);
      }
    };
    return ListExtensions.<Pair<T, List<T>>, T>map(_pairs, _function_1);
  }
  
  /**
   * Returns the position/index of the value in the iterable, starting at 0
   */
  public static <T extends Object> int indexOf(final Iterable<? extends T> iterable, final T value) {
    int counter = 0;
    for (final T o : iterable) {
      {
        boolean _equals = o.equals(value);
        if (_equals) {
          return counter;
        }
        counter = (counter + 1);
      }
    }
    return (-1);
  }
  
  /**
   * map all the some's, leaving alone the none's
   */
  public static <T extends Object, R extends Object> Iterable<Opt<R>> mapOpt(final Iterable<? extends Opt<T>> iterable, final Function1<? super T, ? extends R> fn) {
    final Function1<Opt<T>, Opt<R>> _function = new Function1<Opt<T>, Opt<R>>() {
      public Opt<R> apply(final Opt<T> it) {
        return OptExtensions.<R, T>map(it, fn);
      }
    };
    return org.eclipse.xtext.xbase.lib.IterableExtensions.map(iterable, _function);
  }
  
  /**
   * try to map the values in the iterable, and give back a list of options instead of direct values
   */
  public static <T extends Object, R extends Object> Iterable<Opt<R>> attemptMap(final Iterable<? extends T> iterable, final Function1<? super T, ? extends R> fn) {
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
    return org.eclipse.xtext.xbase.lib.IterableExtensions.map(iterable, _function);
  }
  
  /**
   * transforms a map into a list of pairs
   */
  public static <K extends Object, V extends Object> List<Pair<K, V>> toPairs(final Map<K, V> map) {
    Set<Map.Entry<K, V>> _entrySet = map.entrySet();
    final Function1<Map.Entry<K, V>, Pair<K, V>> _function = new Function1<Map.Entry<K, V>, Pair<K, V>>() {
      public Pair<K, V> apply(final Map.Entry<K, V> it) {
        K _key = it.getKey();
        V _value = it.getValue();
        return Pair.<K, V>of(_key, _value);
      }
    };
    Iterable<Pair<K, V>> _map = org.eclipse.xtext.xbase.lib.IterableExtensions.<Map.Entry<K, V>, Pair<K, V>>map(_entrySet, _function);
    return IterableExtensions.<Pair<K, V>>toList(_map);
  }
  
  /**
   * Convert a list of pairs to a map
   */
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
  
  /**
   * Create a grouped index for a list of items. if the keys are unique, use toMap instead!
   * <p>
   * Example: val groups = levels.groupBy [ difficulty ] // creates a map with per difficulty level a list of levels
   */
  public static <K extends Object, V extends Object> Map<K, List<V>> groupBy(final Iterable<? extends V> list, final Function1<? super V, ? extends K> indexFn) {
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
      org.eclipse.xtext.xbase.lib.IterableExtensions.forEach(list, _function);
      _xblockexpression = map;
    }
    return _xblockexpression;
  }
  
  /**
   * Lets you map a pair using
   */
  public static <K extends Object, V extends Object, R extends Object> List<R> map(final List<Pair<K, V>> list, final Function2<? super K, ? super V, ? extends R> mapFn) {
    final Function1<Pair<K, V>, R> _function = new Function1<Pair<K, V>, R>() {
      public R apply(final Pair<K, V> it) {
        K _key = it.getKey();
        V _value = it.getValue();
        return mapFn.apply(_key, _value);
      }
    };
    return ListExtensions.<Pair<K, V>, R>map(list, _function);
  }
  
  /**
   * Convert to a different type. will throw a class cast exception at runtime
   * if you convert to the wrong type!
   */
  public static <T extends Object> Iterable<T> mapAs(final Iterable<?> iterable, final Class<T> type) {
    final Function1<Object, T> _function = new Function1<Object, T>() {
      public T apply(final Object it) {
        return ((T) it);
      }
    };
    return org.eclipse.xtext.xbase.lib.IterableExtensions.map(iterable, _function);
  }
  
  /**
   * Flatten a list of keys -> list of pair values into more key->value pairs
   */
  public static <K extends Object, V extends Object> Iterable<Pair<K, V>> flattenValues(final Iterable<Pair<K, List<V>>> pairs) {
    LinkedList<Pair<K, V>> _xblockexpression = null;
    {
      final LinkedList<Pair<K, V>> newList = CollectionLiterals.<Pair<K, V>>newLinkedList();
      final Procedure1<Pair<K, List<V>>> _function = new Procedure1<Pair<K, List<V>>>() {
        public void apply(final Pair<K, List<V>> pair) {
          List<V> _value = pair.getValue();
          if (_value!=null) {
            final Procedure1<V> _function = new Procedure1<V>() {
              public void apply(final V it) {
                K _key = pair.getKey();
                Pair<K, V> _mappedTo = Pair.<K, V>of(_key, it);
                newList.add(_mappedTo);
              }
            };
            org.eclipse.xtext.xbase.lib.IterableExtensions.<V>forEach(_value, _function);
          }
        }
      };
      org.eclipse.xtext.xbase.lib.IterableExtensions.<Pair<K, List<V>>>forEach(pairs, _function);
      _xblockexpression = newList;
    }
    return _xblockexpression;
  }
  
  public static <V extends Object> Map<V, Integer> count(final Iterable<? extends V> values) {
    final Function1<V, V> _function = new Function1<V, V>() {
      public V apply(final V it) {
        return it;
      }
    };
    return IterableExtensions.<V, V>countBy(values, _function);
  }
  
  public static <K extends Object, V extends Object> Map<V, Integer> countBy(final Iterable<? extends V> values, final Function1<? super V, ? extends K> indexFn) {
    Map<K, List<V>> _groupBy = IterableExtensions.<K, V>groupBy(values, indexFn);
    List<Pair<K, List<V>>> _pairs = IterableExtensions.<K, List<V>>toPairs(_groupBy);
    final Function1<Pair<K, List<V>>, Pair<V, Integer>> _function = new Function1<Pair<K, List<V>>, Pair<V, Integer>>() {
      public Pair<V, Integer> apply(final Pair<K, List<V>> it) {
        List<V> _value = it.getValue();
        V _head = org.eclipse.xtext.xbase.lib.IterableExtensions.<V>head(_value);
        List<V> _value_1 = it.getValue();
        int _size = _value_1.size();
        return Pair.<V, Integer>of(_head, Integer.valueOf(_size));
      }
    };
    List<Pair<V, Integer>> _map = ListExtensions.<Pair<K, List<V>>, Pair<V, Integer>>map(_pairs, _function);
    return IterableExtensions.<V, Integer>toMap(_map);
  }
  
  public static <K extends Object, V extends Object> Map<K, V> index(final Iterable<? extends V> iterable, final Function1<? super V, ? extends K> indexFn) {
    final Function1<V, K> _function = new Function1<V, K>() {
      public K apply(final V it) {
        return indexFn.apply(it);
      }
    };
    return org.eclipse.xtext.xbase.lib.IterableExtensions.<K, V>toMap(iterable, _function);
  }
  
  public static <T extends Number> double sum(final Iterable<? extends T> values) {
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
  
  public static <T extends Number> double average(final Iterable<? extends T> values) {
    double _sum = IterableExtensions.<T>sum(values);
    int _length = ((Object[])Conversions.unwrapArray(values, Object.class)).length;
    return (_sum / _length);
  }
  
  public static <T extends Object> boolean in(final T instance, final T... objects) {
    boolean _and = false;
    boolean _and_1 = false;
    boolean _defined = OptExtensions.<Object>defined(instance);
    if (!_defined) {
      _and_1 = false;
    } else {
      boolean _defined_1 = OptExtensions.<Object>defined(objects);
      _and_1 = _defined_1;
    }
    if (!_and_1) {
      _and = false;
    } else {
      boolean _contains = ((List<T>)Conversions.doWrapArray(objects)).contains(instance);
      _and = _contains;
    }
    return _and;
  }
  
  public static <T extends Object> boolean in(final T instance, final Iterable<? extends T> objects) {
    List<? extends T> _list = IterableExtensions.toList(objects);
    return _list.contains(instance);
  }
  
  public static <T extends Object> boolean in(final T instance, final Iterator<? extends T> objects) {
    boolean _and = false;
    boolean _and_1 = false;
    boolean _defined = OptExtensions.<Object>defined(instance);
    if (!_defined) {
      _and_1 = false;
    } else {
      boolean _defined_1 = OptExtensions.<Object>defined(objects);
      _and_1 = _defined_1;
    }
    if (!_and_1) {
      _and = false;
    } else {
      List<T> _list = IteratorExtensions.<T>toList(objects);
      boolean _contains = _list.contains(instance);
      _and = _contains;
    }
    return _and;
  }
  
  /**
   * Create a new immutable list from this list that does not contain the value
   */
  public static <T extends Object> ImmutableList<T> operator_minus(final List<? extends T> list, final T value) {
    return IterableExtensions.<T>uncat(list, value);
  }
  
  /**
   * Create a new immutable list from a list and a value
   */
  public static <T extends Object> ImmutableList<T> operator_plus(final Iterable<? extends T> list, final T value) {
    return IterableExtensions.<T>concat(list, value);
  }
  
  /**
   * Create a new immutable list from this list that does not contain the value
   */
  public static <T extends Object> ImmutableList<T> operator_minus(final List<? extends T> list, final List<? extends T> list2) {
    return IterableExtensions.<T>uncat(list, list2);
  }
  
  /**
   * Create a new immutable list from a value and a list
   */
  public static <T extends Object, R extends Object> Iterable<R> operator_mappedTo(final Iterable<? extends T> original, final Function1<? super T, ? extends R> transformation) {
    return org.eclipse.xtext.xbase.lib.IterableExtensions.map(original, transformation);
  }
  
  public static <T extends Object> Iterable<? extends T> operator_plus(final Iterable<? extends T> unfiltered, final Function1<? super T, Boolean> predicate) {
    return org.eclipse.xtext.xbase.lib.IterableExtensions.filter(unfiltered, predicate);
  }
  
  public static <T extends Object> Iterable<? extends T> operator_minus(final Iterable<? extends T> unfiltered, final Function1<? super T, Boolean> predicate) {
    final Function1<T, Boolean> _function = new Function1<T, Boolean>() {
      public Boolean apply(final T it) {
        Boolean _apply = predicate.apply(it);
        return Boolean.valueOf((!(_apply).booleanValue()));
      }
    };
    return org.eclipse.xtext.xbase.lib.IterableExtensions.filter(unfiltered, _function);
  }
  
  public static <T extends Object> Function1<? super T, Boolean> operator_not(final Function1<? super T, Boolean> predicate) {
    final Function1<T, Boolean> _function = new Function1<T, Boolean>() {
      public Boolean apply(final T it) {
        Boolean _apply = predicate.apply(it);
        return Boolean.valueOf((!(_apply).booleanValue()));
      }
    };
    return _function;
  }
}
