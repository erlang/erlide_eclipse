package org.erlide.runtime;

import java.util.Collection;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.hamcrest.Matcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.Test;

@SuppressWarnings("all")
public class RuntimeInfoTest {
  @Test
  public void codePath_Runtime_1() {
    final RuntimeInfo info = new RuntimeInfo("dummy");
    final Collection<String> pa = info.getCodePath();
    int _size = pa.size();
    Matcher<Integer> _is = Matchers.<Integer>is(Integer.valueOf(0));
    MatcherAssert.<Integer>assertThat(Integer.valueOf(_size), _is);
    boolean _isValid = info.isValid();
    Matcher<Boolean> _is_1 = Matchers.<Boolean>is(Boolean.valueOf(false));
    MatcherAssert.<Boolean>assertThat(Boolean.valueOf(_isValid), _is_1);
  }
}
