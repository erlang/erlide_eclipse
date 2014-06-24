package org.erlide.util;

import com.google.common.base.Splitter;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.StringExtensions;

@SuppressWarnings("all")
public class MapCodec {
  public static Map<String, String> decode(final String string) {
    final HashMap<String, String> result = CollectionLiterals.<String, String>newHashMap();
    Splitter _on = Splitter.on("!,");
    final Iterable<String> entries = _on.split(string);
    for (final String entry : entries) {
      boolean _isNullOrEmpty = StringExtensions.isNullOrEmpty(entry);
      boolean _not = (!_isNullOrEmpty);
      if (_not) {
        Splitter _on_1 = Splitter.on(":!");
        final Iterable<String> split = _on_1.split(entry);
        String _head = IterableExtensions.<String>head(split);
        Iterable<String> _tail = IterableExtensions.<String>tail(split);
        String _head_1 = IterableExtensions.<String>head(_tail);
        result.put(_head, _head_1);
      }
    }
    return result;
  }
  
  public static String encode(final Map<String, String> map) {
    StringConcatenation _builder = new StringConcatenation();
    {
      Set<Map.Entry<String, String>> _entrySet = map.entrySet();
      for(final Map.Entry<String, String> e : _entrySet) {
        String _key = e.getKey();
        _builder.append(_key, "");
        _builder.append(":!");
        String _value = e.getValue();
        _builder.append(_value, "");
        _builder.append("!,");
      }
    }
    return _builder.toString();
  }
}
