package org.erlide.util;

import com.google.common.collect.Maps;
import java.util.Collections;
import java.util.Map;
import org.erlide.util.MapCodec;
import org.hamcrest.Matcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.Test;

@SuppressWarnings("all")
public class MapCodecTests {
  @Test
  public void mapShouldRestore() {
    Map<String,String> _xsetliteral = null;
    Map<String,String> _tempMap = Maps.<String, String>newHashMap();
    _tempMap.put("a", "b");
    _tempMap.put(" c ", " d ");
    _tempMap.put("e", "f");
    _xsetliteral = Collections.<String, String>unmodifiableMap(_tempMap);
    final Map<String,String> expected = _xsetliteral;
    final String str = MapCodec.encode(expected);
    final Map<String,String> actual = MapCodec.decode(str);
    Matcher<Map<String,String>> _is = Matchers.<Map<String,String>>is(expected);
    MatcherAssert.<Map<String,String>>assertThat(actual, _is);
  }
}
