package org.erlide.engine.model.erlang.configuration

import org.eclipse.core.runtime.Path
import org.erlide.engine.model.root.ErlangProjectProperties
import org.junit.Before
import org.junit.Test

import static com.google.common.truth.Truth.assertThat
import org.erlide.engine.model.root.RebarConfigurationSerializer

class RebarConfiguratorTest {

    RebarConfigurationSerializer configurator

    @Before
    def void init() {
        configurator = new RebarConfigurationSerializer
    }

    @Test
    def void emptyInputShouldReturnDefaultConfig() {
        val expected = new ErlangProjectProperties
        expected.setOutputDir(new Path("ebin"))
        val actual = configurator.decodeConfig("")
        assertThat(actual).isEqualTo(expected)
    }

    @Test
    def void includeDirectoriesShouldBeConfigured() {
        val input = '''
            {erl_opts, [{i, "myinclude"}]}.
        '''
        val actual = configurator.decodeConfig(input)
        assertThat(actual.getIncludeDirs).contains(
            new Path("myinclude")
        )
    }

    @Test
    def void multipleIncludeDirectoriesShouldBeConfigured() {
        val input = '''
            {erl_opts, [{i, "myinclude"},foo,{i, "myinclude2"}]}.
        '''
        val actual = configurator.decodeConfig(input)
        assertThat(actual.getIncludeDirs).containsAllIn(
            #[new Path("myinclude"), new Path("myinclude2")]
        )
    }

    @Test
    def void sourceDirectoriesShouldBeConfigured() {
        val input = '''
            {erl_opts, [{src_dirs, ["src1", "src2"]}]}.
        '''
        val actual = configurator.decodeConfig(input)
        assertThat(actual.getSourceDirs).containsAllIn(
            #[new Path("src1"), new Path("src2")]
        )
    }

    @Test
    def void outputPathShouldBeConfigured() {
        val input = '''
            {erl_opts, []}.
        '''
        val actual = configurator.decodeConfig(input)
        assertThat(actual.getOutputDir).isEqualTo(new Path("ebin"))
    }

    @Test
    def void handleComplexInput() {
        val input = '''
            something.
            {erl_opts, [{src_dirs, ["src1", "src2"]}]}.
        '''
        val actual = configurator.decodeConfig(input)
        assertThat(actual.getSourceDirs).containsAllIn(
            #[new Path("src1"), new Path("src2")]
        )
    }

    @Test
    def void handleMultipleOptions() {
        val input = '''
            {erl_opts, [{i, "inc1"}]}.
            {erl_opts, [{src_dirs, ["src1", "src2"]}]}.
            {erl_opts, [{i, "inc2"}]}.
            {erl_opts, [{src_dirs, ["src3", "src4"]}]}.
        '''
        val actual = configurator.decodeConfig(input)
        assertThat(actual.getIncludeDirs).containsAllIn(
            #[new Path("inc1"), new Path("inc2")]
        )
        assertThat(actual.getSourceDirs).containsAllIn(
            #[new Path("src3"), new Path("src4")]
        )
    }
}
