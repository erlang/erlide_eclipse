package org.erlide.engine.model.erlang.configuration;

import java.util.Collection;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.erlide.engine.internal.model.root.EmakeConfigurationSerializer;
import org.erlide.engine.model.erlang.ErlangProjectPropertiesMatcher;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.hamcrest.Matcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Test;

@SuppressWarnings("all")
public class EmakeConfiguratorTest {
    private EmakeConfigurationSerializer configurator;

    @Before
    public void init() {
        final EmakeConfigurationSerializer _emakeConfigurationSerializer = new EmakeConfigurationSerializer();
        configurator = _emakeConfigurationSerializer;
    }

    @Test
    public void emptyInputshouldReturnDefaultConfig() {
        final ErlangProjectProperties expected = new ErlangProjectProperties();
        final Path _path = new Path("ebin");
        expected.setOutputDir(_path);
        expected.setSourceDirs();
        final ErlangProjectProperties actual = configurator.decodeConfig("");
        final Matcher<ErlangProjectProperties> _sameAs = ErlangProjectPropertiesMatcher
                .<Object> sameAs(expected);
        MatcherAssert.<ErlangProjectProperties> assertThat(actual, _sameAs);
    }

    @Test
    public void includeDirectoriesShouldBeConfigured() {
        final StringConcatenation _builder = new StringConcatenation();
        _builder.append("{\'src1/*\',[debug_info,{i,\"myinclude\"}]}.");
        _builder.newLine();
        final String input = _builder.toString();
        final ErlangProjectProperties actual = configurator.decodeConfig(input);
        final Collection<IPath> _includeDirs = actual.getIncludeDirs();
        final Path _path = new Path("myinclude");
        final Matcher<Iterable<? extends IPath>> _contains = Matchers
                .<IPath> contains((IPath) _path);
        MatcherAssert.<Collection<IPath>> assertThat(_includeDirs, _contains);
    }

    @Test
    public void multipleIncludeDirectoriesShouldBeConfigured() {
        final StringConcatenation _builder = new StringConcatenation();
        _builder.append(
                "{\'src1/*\',[debug_info,{i, \"myinclude\"}, {i, \"myinclude2\"}]}.");
        _builder.newLine();
        final String input = _builder.toString();
        final ErlangProjectProperties actual = configurator.decodeConfig(input);
        final Collection<IPath> _includeDirs = actual.getIncludeDirs();
        final Path _path = new Path("myinclude");
        final Path _path_1 = new Path("myinclude2");
        final Matcher<Iterable<? extends IPath>> _contains = Matchers
                .<IPath> contains((IPath) _path, (IPath) _path_1);
        MatcherAssert.<Collection<IPath>> assertThat(_includeDirs, _contains);
    }

    @Test
    public void sourceDirectoriesShouldBeConfigured() {
        final StringConcatenation _builder = new StringConcatenation();
        _builder.append("{\'src1/*\',[debug_info]}.");
        _builder.newLine();
        _builder.append("{\'src2/*\',[debug_info]}.");
        _builder.newLine();
        final String input = _builder.toString();
        final ErlangProjectProperties actual = configurator.decodeConfig(input);
        final Collection<IPath> _sourceDirs = actual.getSourceDirs();
        final Path _path = new Path("src1");
        final Path _path_1 = new Path("src2");
        final Matcher<Iterable<? extends IPath>> _contains = Matchers
                .<IPath> contains((IPath) _path, (IPath) _path_1);
        MatcherAssert.<Collection<IPath>> assertThat(_sourceDirs, _contains);
    }

    @Test
    public void outputPathShouldBeConfigured() {
        final StringConcatenation _builder = new StringConcatenation();
        _builder.append("{\'src/*\',[]}.");
        _builder.newLine();
        final String input = _builder.toString();
        final ErlangProjectProperties actual = configurator.decodeConfig(input);
        final IPath _outputDir = actual.getOutputDir();
        final Path _path = new Path("ebin");
        final Matcher<IPath> _is = Matchers.<IPath> is(_path);
        MatcherAssert.<IPath> assertThat(_outputDir, _is);
    }
}
