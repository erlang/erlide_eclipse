package nl.kii.util;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure2;

@SuppressWarnings("all")
public class MapExtensions {
  public static <K extends Object, V extends Object> Map<K, V> operator_add(final Map<K, V> map, final Pair<K, V> pair) {
    Map<K, V> _xblockexpression = null;
    {
      K _key = pair.getKey();
      V _value = pair.getValue();
      map.put(_key, _value);
      _xblockexpression = map;
    }
    return _xblockexpression;
  }
  
  public static <K extends Object, V extends Object> Map<K, V> operator_add(final Map<K, V> map, final Map.Entry<K, V> entry) {
    Map<K, V> _xblockexpression = null;
    {
      K _key = entry.getKey();
      V _value = entry.getValue();
      map.put(_key, _value);
      _xblockexpression = map;
    }
    return _xblockexpression;
  }
  
  public static <K extends Object, K2 extends Object, V extends Object> V add(final Map<K, Map<K2, V>> map, final Pair<K, Pair<K2, V>> pairPair) {
    K _key = pairPair.getKey();
    Pair<K2, V> _value = pairPair.getValue();
    K2 _key_1 = _value.getKey();
    Pair<K2, V> _value_1 = pairPair.getValue();
    V _value_2 = _value_1.getValue();
    return MapExtensions.<K, K2, V>put(map, _key, _key_1, _value_2);
  }
  
  public static <K extends Object, K2 extends Object, V extends Object> V put(final Map<K, Map<K2, V>> map, final K key1, final K2 key2, final V value) {
    V _xblockexpression = null;
    {
      Map<K2, V> _xifexpression = null;
      boolean _containsKey = map.containsKey(key1);
      if (_containsKey) {
        _xifexpression = map.get(key1);
      } else {
        _xifexpression = CollectionLiterals.<K2, V>newHashMap();
      }
      final Map<K2, V> valueMap = _xifexpression;
      _xblockexpression = valueMap.put(key2, value);
    }
    return _xblockexpression;
  }
  
  public static <K extends Object, K2 extends Object, V extends Object> V remove(final Map<K, Map<K2, V>> map, final K key1, final K2 key2) {
    V _xblockexpression = null;
    {
      final Map<K2, V> valueMap = map.get(key1);
      V _xifexpression = null;
      boolean _tripleNotEquals = (valueMap != null);
      if (_tripleNotEquals) {
        _xifexpression = valueMap.remove(key2);
      }
      _xblockexpression = _xifexpression;
    }
    return _xblockexpression;
  }
  
  /**
   * Add a value to a multimap structure
   */
  public static <K extends Object, V extends Object> void add(final Map<K, List<V>> map, final K key, final V value) {
    boolean _containsKey = map.containsKey(key);
    if (_containsKey) {
      List<V> _get = map.get(key);
      _get.add(value);
    } else {
      LinkedList<V> _newLinkedList = CollectionLiterals.<V>newLinkedList();
      map.put(key, _newLinkedList);
      MapExtensions.<K, V>add(map, key, value);
    }
  }
  
  /**
   * Iterate through a map as if it were a list, but with key and value
   */
  public static <K extends Object, V extends Object> Map<K, V> forEach(final Map<K, V> map, final Procedure2<? super K, ? super V> fn) {
    Map<K, V> _xblockexpression = null;
    {
      Set<Map.Entry<K, V>> _entrySet = map.entrySet();
      final Function1<Map.Entry<K, V>, Pair<K, V>> _function = new Function1<Map.Entry<K, V>, Pair<K, V>>() {
        public Pair<K, V> apply(final Map.Entry<K, V> it) {
          K _key = it.getKey();
          V _value = it.getValue();
          return Pair.<K, V>of(_key, _value);
        }
      };
      Iterable<Pair<K, V>> _map = IterableExtensions.<Map.Entry<K, V>, Pair<K, V>>map(_entrySet, _function);
      final Procedure1<Pair<K, V>> _function_1 = new Procedure1<Pair<K, V>>() {
        public void apply(final Pair<K, V> it) {
          K _key = it.getKey();
          V _value = it.getValue();
          fn.apply(_key, _value);
        }
      };
      IterableExtensions.<Pair<K, V>>forEach(_map, _function_1);
      _xblockexpression = map;
    }
    return _xblockexpression;
  }
}
