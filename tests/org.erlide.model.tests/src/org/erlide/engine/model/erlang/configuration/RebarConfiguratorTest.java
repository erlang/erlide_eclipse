package org.erlide.engine.model.erlang.configuration;

import java.util.Collections;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.RebarConfigurationSerializer;
import org.junit.Before;
import org.junit.Test;

import com.google.common.truth.IterableSubject;
import com.google.common.truth.Subject;
import com.google.common.truth.Truth;

@SuppressWarnings("all")
public class RebarConfiguratorTest {
    private RebarConfigurationSerializer configurator;

    @Before
    public void init() {
        final RebarConfigurationSerializer _rebarConfigurationSerializer = new RebarConfigurationSerializer();
        configurator = _rebarConfigurationSerializer;
    }

    @Test
    public void emptyInputShouldReturnDefaultConfig() {
        final ErlangProjectProperties expected = new ErlangProjectProperties();
        final Path _path = new Path("ebin");
        expected.setOutputDir(_path);
        final ErlangProjectProperties actual = configurator.decodeConfig("");
        Truth.assertThat(actual).isEqualTo(expected);
    }

    @Test
    public void includeDirectoriesShouldBeConfigured() {
        final StringConcatenation _builder = new StringConcatenation();
        _builder.append("{erl_opts, [{i, \"myinclude\"}]}.");
        _builder.newLine();
        final String input = _builder.toString();
        final ErlangProjectProperties actual = configurator.decodeConfig(input);
        final IterableSubject _assertThat = Truth
                .<IPath, Iterable<IPath>> assertThat(actual.getIncludeDirs());
        final Path _path = new Path("myinclude");
        _assertThat.contains(_path);
    }

    @Test
    public void multipleIncludeDirectoriesShouldBeConfigured() {
        final StringConcatenation _builder = new StringConcatenation();
        _builder.append("{erl_opts, [{i, \"myinclude\"},foo,{i, \"myinclude2\"}]}.");
        _builder.newLine();
        final String input = _builder.toString();
        final ErlangProjectProperties actual = configurator.decodeConfig(input);
        final Path _path = new Path("myinclude");
        final Path _path_1 = new Path("myinclude2");
        Truth.<IPath, Iterable<IPath>> assertThat(actual.getIncludeDirs())
                .containsAtLeastElementsIn(Collections.<Object> unmodifiableList(
                        CollectionLiterals.<Object> newArrayList(_path, _path_1)));
    }

    @Test
    public void sourceDirectoriesShouldBeConfigured() {
        final StringConcatenation _builder = new StringConcatenation();
        _builder.append("{erl_opts, [{src_dirs, [\"src1\", \"src2\"]}]}.");
        _builder.newLine();
        final String input = _builder.toString();
        final ErlangProjectProperties actual = configurator.decodeConfig(input);
        final Path _path = new Path("src1");
        final Path _path_1 = new Path("src2");
        Truth.<IPath, Iterable<IPath>> assertThat(actual.getSourceDirs())
                .containsAtLeastElementsIn(Collections.<Object> unmodifiableList(
                        CollectionLiterals.<Object> newArrayList(_path, _path_1)));
    }

    @Test
    public void outputPathShouldBeConfigured() {
        final StringConcatenation _builder = new StringConcatenation();
        _builder.append("{erl_opts, []}.");
        _builder.newLine();
        final String input = _builder.toString();
        final ErlangProjectProperties actual = configurator.decodeConfig(input);
        final Subject _assertThat = Truth.assertThat(actual.getOutputDir());
        final Path _path = new Path("ebin");
        _assertThat.isEqualTo(_path);
    }

    @Test
    public void handleComplexInput() {
        final StringConcatenation _builder = new StringConcatenation();
        _builder.append("something.");
        _builder.newLine();
        _builder.append("{erl_opts, [{src_dirs, [\"src1\", \"src2\"]}]}.");
        _builder.newLine();
        final String input = _builder.toString();
        final ErlangProjectProperties actual = configurator.decodeConfig(input);
        final Path _path = new Path("src1");
        final Path _path_1 = new Path("src2");
        Truth.<IPath, Iterable<IPath>> assertThat(actual.getSourceDirs())
                .containsAtLeastElementsIn(Collections.<Object> unmodifiableList(
                        CollectionLiterals.<Object> newArrayList(_path, _path_1)));
    }

    @Test
    public void handleMultipleOptions() {
        final StringConcatenation _builder = new StringConcatenation();
        _builder.append("{erl_opts, [{i, \"inc1\"}]}.");
        _builder.newLine();
        _builder.append("{erl_opts, [{src_dirs, [\"src1\", \"src2\"]}]}.");
        _builder.newLine();
        _builder.append("{erl_opts, [{i, \"inc2\"}]}.");
        _builder.newLine();
        _builder.append("{erl_opts, [{src_dirs, [\"src3\", \"src4\"]}]}.");
        _builder.newLine();
        final String input = _builder.toString();
        final ErlangProjectProperties actual = configurator.decodeConfig(input);
        final Path _path = new Path("inc1");
        final Path _path_1 = new Path("inc2");
        Truth.<IPath, Iterable<IPath>> assertThat(actual.getIncludeDirs())
                .containsAtLeastElementsIn(Collections.<Object> unmodifiableList(
                        CollectionLiterals.<Object> newArrayList(_path, _path_1)));
        final Path _path_2 = new Path("src3");
        final Path _path_3 = new Path("src4");
        Truth.<IPath, Iterable<IPath>> assertThat(actual.getSourceDirs())
                .containsAtLeastElementsIn(Collections.<Object> unmodifiableList(
                        CollectionLiterals.<Object> newArrayList(_path_2, _path_3)));
    }
}
