package org.erlide.core.services.builder;

import java.util.Collection;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.core.internal.builder.external.RebarConfigurator;
import org.erlide.engine.model.erlang.ErlangProjectPropertiesMatcher;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.hamcrest.Matcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Test;

@SuppressWarnings("all")
public class RebarConfiguratorTests {
  private RebarConfigurator configurator;
  
  @Before
  public void init() {
    RebarConfigurator _rebarConfigurator = new RebarConfigurator();
    this.configurator = _rebarConfigurator;
  }
  
  @Test
  public void emptyInputshouldReturnDefaultConfig() {
    ErlangProjectProperties _erlangProjectProperties = new ErlangProjectProperties();
    final ErlangProjectProperties expected = _erlangProjectProperties;
    final ErlangProjectProperties actual = this.configurator.decodeConfig("");
    Matcher<ErlangProjectProperties> _sameAs = ErlangProjectPropertiesMatcher.<Object>sameAs(expected);
    MatcherAssert.<ErlangProjectProperties>assertThat(actual, _sameAs);
  }
  
  @Test
  public void includeDirectoriesShouldBeConfigured() {
    final String input = "{erl_opts, [{i, \"myinclude\"}]}.";
    final ErlangProjectProperties actual = this.configurator.decodeConfig(input);
    Collection<IPath> _includeDirs = actual.getIncludeDirs();
    Path _path = new Path("myinclude");
    Matcher<Iterable<? extends IPath>> _contains = Matchers.<IPath>contains(((IPath) _path));
    MatcherAssert.<Collection<IPath>>assertThat(_includeDirs, _contains);
  }
  
  @Test
  public void sourceDirectoriesShouldBeConfigured() {
    final String input = "{erl_opts, [{src_dirs, [\"src1\", \"src2\"]}]}.";
    final ErlangProjectProperties actual = this.configurator.decodeConfig(input);
    Collection<IPath> _sourceDirs = actual.getSourceDirs();
    Path _path = new Path("src1");
    Path _path_1 = new Path("src2");
    Matcher<Iterable<? extends IPath>> _contains = Matchers.<IPath>contains(((IPath) _path), ((IPath) _path_1));
    MatcherAssert.<Collection<IPath>>assertThat(_sourceDirs, _contains);
  }
  
  @Test
  public void outputPathShouldBeConfigured() {
    final String input = "{erl_opts, []}.";
    final ErlangProjectProperties actual = this.configurator.decodeConfig(input);
    IPath _outputDir = actual.getOutputDir();
    Path _path = new Path("ebin");
    Matcher<IPath> _is = Matchers.<IPath>is(_path);
    MatcherAssert.<IPath>assertThat(_outputDir, _is);
  }
  
  @Test
  public void handleComplexInput() {
    final String input = "something. {erl_opts, [{src_dirs, [\"src1\", \"src2\"]}]}.";
    final ErlangProjectProperties actual = this.configurator.decodeConfig(input);
    Collection<IPath> _sourceDirs = actual.getSourceDirs();
    Path _path = new Path("src1");
    Path _path_1 = new Path("src2");
    Matcher<Iterable<? extends IPath>> _contains = Matchers.<IPath>contains(((IPath) _path), ((IPath) _path_1));
    MatcherAssert.<Collection<IPath>>assertThat(_sourceDirs, _contains);
  }
}
