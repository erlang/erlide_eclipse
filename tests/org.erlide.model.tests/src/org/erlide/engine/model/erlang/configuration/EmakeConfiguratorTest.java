package org.erlide.engine.model.erlang.configuration;

import java.util.Collections;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.erlide.engine.model.root.EmakeConfigurationSerializer;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.junit.Before;
import org.junit.Test;

import com.google.common.truth.DefaultSubject;
import com.google.common.truth.IterableSubject;
import com.google.common.truth.Subject;
import com.google.common.truth.Truth;

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
        Truth.assertThat(actual).isEqualTo(expected);
    }

    @Test
    public void includeDirectoriesShouldBeConfigured() {
        final StringConcatenation _builder = new StringConcatenation();
        _builder.append("{\'src1/*\',[debug_info,{i,\"myinclude\"}]}.");
        _builder.newLine();
        final String input = _builder.toString();
        final ErlangProjectProperties actual = configurator.decodeConfig(input);
        final IterableSubject<? extends IterableSubject<?, IPath, Iterable<IPath>>, IPath, Iterable<IPath>> _assertThat = Truth
                .<IPath, Iterable<IPath>> assertThat(actual.getIncludeDirs());
        final Path _path = new Path("myinclude");
        _assertThat.contains(_path);
    }

    @Test
    public void multipleIncludeDirectoriesShouldBeConfigured() {
        final StringConcatenation _builder = new StringConcatenation();
        _builder.append(
                "{\'src1/*\',[debug_info,{i, \"myinclude\"}, {i, \"myinclude2\"}]}.");
        _builder.newLine();
        final String input = _builder.toString();
        final ErlangProjectProperties actual = configurator.decodeConfig(input);
        final Path _path = new Path("myinclude");
        final Path _path_1 = new Path("myinclude2");
        Truth.<IPath, Iterable<IPath>> assertThat(actual.getIncludeDirs())
                .containsAllIn(Collections.<Object> unmodifiableList(
                        CollectionLiterals.<Object> newArrayList(_path, _path_1)));
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
        final Path _path = new Path("src1");
        final Path _path_1 = new Path("src2");
        Truth.<IPath, Iterable<IPath>> assertThat(actual.getSourceDirs())
                .containsAllIn(Collections.<Object> unmodifiableList(
                        CollectionLiterals.<Object> newArrayList(_path, _path_1)));
    }

    @Test
    public void outputPathShouldBeConfigured() {
        final StringConcatenation _builder = new StringConcatenation();
        _builder.append("{\'src/*\',[]}.");
        _builder.newLine();
        final String input = _builder.toString();
        final ErlangProjectProperties actual = configurator.decodeConfig(input);
        final Subject<DefaultSubject, Object> _assertThat = Truth
                .assertThat(actual.getOutputDir());
        final Path _path = new Path("ebin");
        _assertThat.isEqualTo(_path);
    }
}
