package org.erlide.core.services.builder;

import java.util.List;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.ProgressMonitorWrapper;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.erlide.core.internal.builder.BuildNotifier;
import org.erlide.core.internal.builder.BuildPhase;
import org.hamcrest.Matcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Test;

@SuppressWarnings("all")
public class BuildNotifierTest {
  private final ProgressMonitorWrapper mon = new ProgressMonitorWrapper(new NullProgressMonitor()) {
    @Override
    public void worked(final int work) {
      BuildNotifierTest.this.trace(work);
    }
  };
  
  private final List<Integer> crtTrace = CollectionLiterals.<Integer>newArrayList();
  
  private BuildNotifier notifier;
  
  public void trace(final int text) {
    this.crtTrace.add(Integer.valueOf(text));
  }
  
  @Before
  public void before() {
    this.crtTrace.clear();
    BuildNotifier _buildNotifier = new BuildNotifier(this.mon, null);
    this.notifier = _buildNotifier;
  }
  
  @Test
  public void clean_phase() {
    this.notifier.begin();
    this.notifier.newPhase("clean");
    this.notifier.done();
    int _work = BuildPhase.CLEAN.getWork();
    final int w = (_work * 10);
    Matcher<Iterable<? extends Integer>> _contains = Matchers.<Integer>contains(Integer.valueOf(w), Integer.valueOf((1000 - w)));
    MatcherAssert.<List<Integer>>assertThat(
      this.crtTrace, _contains);
  }
  
  @Test
  public void compile_phase() {
    this.notifier.begin();
    this.notifier.newPhase("compile");
    this.notifier.done();
    int _work = BuildPhase.COMPILE.getWork();
    final int w = (_work * 10);
    Matcher<Iterable<? extends Integer>> _contains = Matchers.<Integer>contains(Integer.valueOf(w), Integer.valueOf((1000 - w)));
    MatcherAssert.<List<Integer>>assertThat(
      this.crtTrace, _contains);
  }
  
  @Test
  public void two_phases() {
    this.notifier.begin();
    this.notifier.newPhase("clean");
    this.notifier.newPhase("compile");
    this.notifier.done();
    int _work = BuildPhase.CLEAN.getWork();
    final int w = (_work * 10);
    int _work_1 = BuildPhase.COMPILE.getWork();
    final int w1 = (_work_1 * 10);
    Matcher<Iterable<? extends Integer>> _contains = Matchers.<Integer>contains(Integer.valueOf(w), Integer.valueOf(w1), Integer.valueOf(((1000 - w) - w1)));
    MatcherAssert.<List<Integer>>assertThat(
      this.crtTrace, _contains);
  }
  
  @Test
  public void compile_steps() {
    this.notifier.begin();
    this.notifier.newPhase("compile");
    this.notifier.newStep(".foo", 0);
    this.notifier.newStep(".zax", 2);
    this.notifier.compiled("foo1");
    this.notifier.compiled("foo2");
    this.notifier.newStep(".erl", 4);
    this.notifier.compiled("erl1");
    this.notifier.compiled("erl2");
    this.notifier.compiled("erl3");
    this.notifier.compiled("erl4");
    this.notifier.newStep(".bar", 0);
    this.notifier.done();
    int _work = BuildPhase.COMPILE.getWork();
    final int w = (_work * 10);
    final int s1 = (w / 100);
    final int s1c = (s1 / 2);
    final int s2 = ((w * 91) / 100);
    final int s2c = (s2 / 4);
    Matcher<Iterable<? extends Integer>> _contains = Matchers.<Integer>contains(Integer.valueOf(s1), Integer.valueOf(s1c), Integer.valueOf((s1c + 1)), Integer.valueOf(s2c), Integer.valueOf((s2c + 1)), Integer.valueOf((s2c + 1)), Integer.valueOf(((s2 - (3 * s2c)) - 2)), Integer.valueOf((s1 + 1)), Integer.valueOf(39), Integer.valueOf((1000 - w)));
    MatcherAssert.<List<Integer>>assertThat(
      this.crtTrace, _contains);
  }
}
