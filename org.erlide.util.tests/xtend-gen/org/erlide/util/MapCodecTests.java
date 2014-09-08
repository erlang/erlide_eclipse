package org.erlide.util;

import java.util.Collections;
import java.util.Map;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Pair;
import org.erlide.util.MapCodec;
import org.hamcrest.Matcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.Test;

@SuppressWarnings("all")
public class MapCodecTests {
  @Test
  public void mapShouldRestore() {
    Pair<String, String> _mappedTo = Pair.<String, String>of("a", "b");
    Pair<String, String> _mappedTo_1 = Pair.<String, String>of(" c ", " d ");
    Pair<String, String> _mappedTo_2 = Pair.<String, String>of("e", "f");
    final Map<String, String> expected = Collections.<String, String>unmodifiableMap(CollectionLiterals.<String, String>newHashMap(_mappedTo, _mappedTo_1, _mappedTo_2));
    final String str = MapCodec.encode(expected);
    final Map<String, String> actual = MapCodec.decode(str);
    Matcher<Map<String, String>> _is = Matchers.<Map<String, String>>is(expected);
    MatcherAssert.<Map<String, String>>assertThat(actual, _is);
  }
}
