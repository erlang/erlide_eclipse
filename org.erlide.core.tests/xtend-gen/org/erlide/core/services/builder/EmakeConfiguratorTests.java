package org.erlide.core.services.builder;

import java.util.Collection;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.erlide.core.internal.builder.external.EmakeConfigurator;
import org.erlide.engine.model.erlang.ErlangProjectPropertiesMatcher;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.hamcrest.Matcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Test;

@SuppressWarnings("all")
public class EmakeConfiguratorTests {
  private EmakeConfigurator configurator;
  
  @Before
  public void init() {
    EmakeConfigurator _emakeConfigurator = new EmakeConfigurator();
    this.configurator = _emakeConfigurator;
  }
  
  @Test
  public void emptyInputshouldReturnDefaultConfig() {
    ErlangProjectProperties _erlangProjectProperties = new ErlangProjectProperties();
    final ErlangProjectProperties expected = _erlangProjectProperties;
    expected.setSourceDirs();
    final ErlangProjectProperties actual = this.configurator.decodeConfig("");
    Matcher<ErlangProjectProperties> _sameAs = ErlangProjectPropertiesMatcher.<Object>sameAs(expected);
    MatcherAssert.<ErlangProjectProperties>assertThat(actual, _sameAs);
  }
  
  @Test
  public void includeDirectoriesShouldBeConfigured() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("{\'src1/*\',[debug_info,{i,\"myinclude\"}]}.");
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
    _builder.append("{\'src1/*\',[debug_info,{i, \"myinclude\"}, {i, \"myinclude2\"}]}.");
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
    _builder.append("{\'src1/*\',[debug_info]}.");
    _builder.newLine();
    _builder.append("{\'src2/*\',[debug_info]}.");
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
    _builder.append("{\'src/*\',[]}.");
    _builder.newLine();
    final String input = _builder.toString();
    final ErlangProjectProperties actual = this.configurator.decodeConfig(input);
    IPath _outputDir = actual.getOutputDir();
    Path _path = new Path("ebin");
    Matcher<IPath> _is = Matchers.<IPath>is(_path);
    MatcherAssert.<IPath>assertThat(_outputDir, _is);
  }
}
