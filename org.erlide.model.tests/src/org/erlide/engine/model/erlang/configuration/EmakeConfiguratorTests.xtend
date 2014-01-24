package org.erlide.engine.model.erlang.configuration

import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.Path
import org.erlide.engine.internal.model.root.EmakeConfigurationSerializer
import org.erlide.engine.model.root.ErlangProjectProperties
import org.junit.Before
import org.junit.Test

import static org.erlide.engine.model.erlang.ErlangProjectPropertiesMatcher.*
import static org.hamcrest.MatcherAssert.*
import static org.hamcrest.Matchers.*

class EmakeConfiguratorTests {

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
        assertThat(actual, sameAs(expected))
    }

    @Test
    def void includeDirectoriesShouldBeConfigured() {
        val input = '''
            {'src1/*',[debug_info,{i,"myinclude"}]}.
        '''
        val actual = configurator.decodeConfig(input)
        assertThat(actual.getIncludeDirs,
            contains(
                new Path("myinclude") as IPath
            ))
    }

    @Test
    def void multipleIncludeDirectoriesShouldBeConfigured() {
        val input = '''
            {'src1/*',[debug_info,{i, "myinclude"}, {i, "myinclude2"}]}.
        '''
        val actual = configurator.decodeConfig(input)
        assertThat(actual.getIncludeDirs,
            contains(
                new Path("myinclude") as IPath,
                new Path("myinclude2") as IPath
            ))
    }

    @Test
    def void sourceDirectoriesShouldBeConfigured() {
        val input = '''
            {'src1/*',[debug_info]}.
            {'src2/*',[debug_info]}.
        '''
        val actual = configurator.decodeConfig(input)
        assertThat(actual.getSourceDirs,
            contains(
                new Path("src1") as IPath,
                new Path("src2") as IPath
            ))
    }

    @Test
    def void outputPathShouldBeConfigured() {
        val input = '''
            {'src/*',[]}.
        '''
        val actual = configurator.decodeConfig(input)
        assertThat(actual.getOutputDir, is(new Path("ebin")))
    }

}
