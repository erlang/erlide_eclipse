package org.erlide.util;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.StringExtensions;

import com.google.common.base.Splitter;

@SuppressWarnings("all")
public class MapCodec {
    public static Map<String, String> decode(final String string) {
        final HashMap<String, String> result = CollectionLiterals
                .<String, String> newHashMap();
        final Iterable<String> entries = Splitter.on("!,").split(string);
        for (final String entry : entries) {
            final boolean _isNullOrEmpty = StringExtensions.isNullOrEmpty(entry);
            final boolean _not = !_isNullOrEmpty;
            if (_not) {
                final Iterable<String> split = Splitter.on(":!").split(entry);
                result.put(IterableExtensions.<String> head(split), IterableExtensions
                        .<String> head(IterableExtensions.<String> tail(split)));
            }
        }
        return result;
    }

    public static String encode(final Map<String, String> map) {
        final StringConcatenation _builder = new StringConcatenation();
        {
            final Set<Map.Entry<String, String>> _entrySet = map.entrySet();
            for (final Map.Entry<String, String> e : _entrySet) {
                final String _key = e.getKey();
                _builder.append(_key);
                _builder.append(":!");
                final String _value = e.getValue();
                _builder.append(_value);
                _builder.append("!,");
            }
        }
        return _builder.toString();
    }
}
