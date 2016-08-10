package org.erlide.engine.model.erlang.configuration

import org.eclipse.core.runtime.Path
import org.erlide.engine.model.root.ErlangProjectProperties
import org.junit.Before
import org.junit.Test

import static com.google.common.truth.Truth.assertThat
import org.erlide.engine.model.root.EmakeConfigurationSerializer

class EmakeConfiguratorTest {

    EmakeConfigurationSerializer configurator

    @Before
    def void init() {
        configurator = new EmakeConfigurationSerializer
    }

    @Test
    def void emptyInputshouldReturnDefaultConfig() {
        val expected = new ErlangProjectProperties
        expected.setOutputDir(new Path("ebin"))
        expected.setSourceDirs()
        val actual = configurator.decodeConfig("")
        assertThat(actual).isSameAs(expected)
    }

    @Test
    def void includeDirectoriesShouldBeConfigured() {
        val input = '''
            {'src1/*',[debug_info,{i,"myinclude"}]}.
        '''
        val actual = configurator.decodeConfig(input)
        assertThat(actual.getIncludeDirs).contains(
            new Path("myinclude")
        )
    }

    @Test
    def void multipleIncludeDirectoriesShouldBeConfigured() {
        val input = '''
            {'src1/*',[debug_info,{i, "myinclude"}, {i, "myinclude2"}]}.
        '''
        val actual = configurator.decodeConfig(input)
        assertThat(actual.getIncludeDirs).containsAllIn(
            #[new Path("myinclude"), new Path("myinclude2")]
        )
    }

    @Test
    def void sourceDirectoriesShouldBeConfigured() {
        val input = '''
            {'src1/*',[debug_info]}.
            {'src2/*',[debug_info]}.
        '''
        val actual = configurator.decodeConfig(input)
        assertThat(actual.getSourceDirs).containsAllIn(
            #[new Path("src1"), new Path("src2")]
        )
    }

    @Test
    def void outputPathShouldBeConfigured() {
        val input = '''
            {'src/*',[]}.
        '''
        val actual = configurator.decodeConfig(input)
        assertThat(actual.getOutputDir).isEqualTo(new Path("ebin"))
    }

}
