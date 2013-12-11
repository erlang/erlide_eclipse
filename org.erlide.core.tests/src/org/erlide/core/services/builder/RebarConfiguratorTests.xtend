package org.erlide.core.services.builder

import org.erlide.core.internal.builder.external.RebarConfigurator
import org.erlide.engine.model.root.ErlangProjectProperties
import org.junit.Before
import org.junit.Test

import static org.hamcrest.MatcherAssert.*
import static org.hamcrest.Matchers.*
import static org.erlide.engine.model.erlang.ErlangProjectPropertiesMatcher.*
import org.eclipse.core.runtime.Path
import org.eclipse.core.runtime.IPath

class RebarConfiguratorTests {

    RebarConfigurator configurator

    @Before
    def void init() {
        configurator = new RebarConfigurator
    }

    @Test
    def void emptyInputShouldReturnDefaultConfig() {
        val expected = new ErlangProjectProperties
        expected.setOutputDir(new Path("ebin"))
        val actual = configurator.decodeConfig("")
        assertThat(actual, sameAs(expected))
    }

    @Test
    def void includeDirectoriesShouldBeConfigured() {
        val input = '''
            {erl_opts, [{i, "myinclude"}]}.
        '''
        val actual = configurator.decodeConfig(input)
        assertThat(actual.includeDirs,
            contains(
                new Path("myinclude") as IPath
            ))
    }

    @Test
    def void multipleIncludeDirectoriesShouldBeConfigured() {
        val input = '''
            {erl_opts, [{i, "myinclude"},foo,{i, "myinclude2"}]}.
        '''
        val actual = configurator.decodeConfig(input)
        assertThat(actual.includeDirs,
            contains(
                new Path("myinclude") as IPath,
                new Path("myinclude2") as IPath
            ))
    }

    @Test
    def void sourceDirectoriesShouldBeConfigured() {
        val input = '''
            {erl_opts, [{src_dirs, ["src1", "src2"]}]}.
        '''
        val actual = configurator.decodeConfig(input)
        assertThat(actual.sourceDirs,
            contains(
                new Path("src1") as IPath,
                new Path("src2") as IPath
            ))
    }

    @Test
    def void outputPathShouldBeConfigured() {
        val input = '''
            {erl_opts, []}.
        ''' 
        val actual = configurator.decodeConfig(input)
        assertThat(actual.outputDir, is(new Path("ebin")))
    }

    @Test
    def void handleComplexInput() {
        val input = '''
            something. 
            {erl_opts, [{src_dirs, ["src1", "src2"]}]}.
        '''
        val actual = configurator.decodeConfig(input)
        assertThat(actual.sourceDirs,
            contains(
                new Path("src1") as IPath,
                new Path("src2") as IPath
            ))
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
        assertThat(actual.includeDirs,
            contains(
                new Path("inc1") as IPath,
                new Path("inc2") as IPath
            ))
        assertThat(actual.sourceDirs,
            contains(
                new Path("src3") as IPath,
                new Path("src4") as IPath
            ))
    }
}
