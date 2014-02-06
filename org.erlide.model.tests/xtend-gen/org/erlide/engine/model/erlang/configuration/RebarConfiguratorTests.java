package org.erlide.engine.model.erlang.configuration;

import java.util.Collection;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.erlide.engine.internal.model.root.RebarConfigurationSerializer;
import org.erlide.engine.model.erlang.ErlangProjectPropertiesMatcher;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.hamcrest.Matcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Test;

@SuppressWarnings("all")
public class RebarConfiguratorTests {
  private RebarConfigurationSerializer configurator;
  
  @Before
  public void init() {
    RebarConfigurationSerializer _rebarConfigurationSerializer = new RebarConfigurationSerializer();
    this.configurator = _rebarConfigurationSerializer;
  }
  
  @Test
  public void emptyInputShouldReturnDefaultConfig() {
    final ErlangProjectProperties expected = new ErlangProjectProperties();
    Path _path = new Path("ebin");
    expected.setOutputDir(_path);
    final ErlangProjectProperties actual = this.configurator.decodeConfig("");
    Matcher<ErlangProjectProperties> _sameAs = ErlangProjectPropertiesMatcher.<Object>sameAs(expected);
    MatcherAssert.<ErlangProjectProperties>assertThat(actual, _sameAs);
  }
  
  @Test
  public void includeDirectoriesShouldBeConfigured() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("{erl_opts, [{i, \"myinclude\"}]}.");
    _builder.newLine();
    final String input = _builder.toString();
    final ErlangProjectProperties actual = this.configurator.decodeConfig(input);
    Collection<IPath> _includeDirs = actual.getIncludeDirs();
    Path _path = new Path("myinclude");
    Matcher<Iterable<? extends IPath>> _contains = Matchers.<IPath>contains(
      ((IPath) _path));
    MatcherAssert.<Collection<IPath>>assertThat(_includeDirs, _contains);
  }
  
  @Test
  public void multipleIncludeDirectoriesShouldBeConfigured() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("{erl_opts, [{i, \"myinclude\"},foo,{i, \"myinclude2\"}]}.");
    _builder.newLine();
    final String input = _builder.toString();
    final ErlangProjectProperties actual = this.configurator.decodeConfig(input);
    Collection<IPath> _includeDirs = actual.getIncludeDirs();
    Path _path = new Path("myinclude");
    Path _path_1 = new Path("myinclude2");
    Matcher<Iterable<? extends IPath>> _contains = Matchers.<IPath>contains(
      ((IPath) _path), 
      ((IPath) _path_1));
    MatcherAssert.<Collection<IPath>>assertThat(_includeDirs, _contains);
  }
  
  @Test
  public void sourceDirectoriesShouldBeConfigured() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("{erl_opts, [{src_dirs, [\"src1\", \"src2\"]}]}.");
    _builder.newLine();
    final String input = _builder.toString();
    final ErlangProjectProperties actual = this.configurator.decodeConfig(input);
    Collection<IPath> _sourceDirs = actual.getSourceDirs();
    Path _path = new Path("src1");
    Path _path_1 = new Path("src2");
    Matcher<Iterable<? extends IPath>> _contains = Matchers.<IPath>contains(
      ((IPath) _path), 
      ((IPath) _path_1));
    MatcherAssert.<Collection<IPath>>assertThat(_sourceDirs, _contains);
  }
  
  @Test
  public void outputPathShouldBeConfigured() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("{erl_opts, []}.");
    _builder.newLine();
    final String input = _builder.toString();
    final ErlangProjectProperties actual = this.configurator.decodeConfig(input);
    IPath _outputDir = actual.getOutputDir();
    Path _path = new Path("ebin");
    Matcher<IPath> _is = Matchers.<IPath>is(_path);
    MatcherAssert.<IPath>assertThat(_outputDir, _is);
  }
  
  @Test
  public void handleComplexInput() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("something.");
    _builder.newLine();
    _builder.append("{erl_opts, [{src_dirs, [\"src1\", \"src2\"]}]}.");
    _builder.newLine();
    final String input = _builder.toString();
    final ErlangProjectProperties actual = this.configurator.decodeConfig(input);
    Collection<IPath> _sourceDirs = actual.getSourceDirs();
    Path _path = new Path("src1");
    Path _path_1 = new Path("src2");
    Matcher<Iterable<? extends IPath>> _contains = Matchers.<IPath>contains(
      ((IPath) _path), 
      ((IPath) _path_1));
    MatcherAssert.<Collection<IPath>>assertThat(_sourceDirs, _contains);
  }
  
  @Test
  public void handleMultipleOptions() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("{erl_opts, [{i, \"inc1\"}]}.");
    _builder.newLine();
    _builder.append("{erl_opts, [{src_dirs, [\"src1\", \"src2\"]}]}.");
    _builder.newLine();
    _builder.append("{erl_opts, [{i, \"inc2\"}]}.");
    _builder.newLine();
    _builder.append("{erl_opts, [{src_dirs, [\"src3\", \"src4\"]}]}.");
    _builder.newLine();
    final String input = _builder.toString();
    final ErlangProjectProperties actual = this.configurator.decodeConfig(input);
    Collection<IPath> _includeDirs = actual.getIncludeDirs();
    Path _path = new Path("inc1");
    Path _path_1 = new Path("inc2");
    Matcher<Iterable<? extends IPath>> _contains = Matchers.<IPath>contains(
      ((IPath) _path), 
      ((IPath) _path_1));
    MatcherAssert.<Collection<IPath>>assertThat(_includeDirs, _contains);
    Collection<IPath> _sourceDirs = actual.getSourceDirs();
    Path _path_2 = new Path("src3");
    Path _path_3 = new Path("src4");
    Matcher<Iterable<? extends IPath>> _contains_1 = Matchers.<IPath>contains(
      ((IPath) _path_2), 
      ((IPath) _path_3));
    MatcherAssert.<Collection<IPath>>assertThat(_sourceDirs, _contains_1);
  }
}
