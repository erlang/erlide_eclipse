package org.erlide.runtime;

import java.util.Collection;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.erlide.runtime.runtimeinfo.RuntimeFinder;
import org.erlide.util.SystemConfiguration;
import org.hamcrest.Matcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.Test;

@SuppressWarnings("all")
public class RuntimeFinderTest {
  @Test
  public void findKerlRuntimes() {
    SystemConfiguration _instance = SystemConfiguration.getInstance();
    boolean _isOnWindows = _instance.isOnWindows();
    if (_isOnWindows) {
      return;
    }
    final Collection<String> kerl = RuntimeFinder.getKerlLocations();
    int _length = ((Object[])Conversions.unwrapArray(kerl, Object.class)).length;
    Matcher<Integer> _greaterThan = Matchers.<Integer>greaterThan(Integer.valueOf(0));
    Matcher<Integer> _is = Matchers.<Integer>is(_greaterThan);
    MatcherAssert.<Integer>assertThat(Integer.valueOf(_length), _is);
  }
}
