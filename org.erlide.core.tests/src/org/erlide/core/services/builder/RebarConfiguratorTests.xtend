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
    def void emptyInputshouldReturnDefaultConfig() {
        val expected = new ErlangProjectProperties
        val actual = configurator.decodeConfig("")
        assertThat(actual, sameAs(expected))
    }

    @Test
    def void includeDirectoriesShouldBeConfigured() {
        val input = "{erl_opts, [{i, \"myinclude\"}]}."
        val actual = configurator.decodeConfig(input)
        assertThat(actual.includeDirs, contains(new Path("myinclude") as IPath))
    }

    @Test
    def void sourceDirectoriesShouldBeConfigured() {
        val input = "{erl_opts, [{src_dirs, [\"src\", \"src2\"]}]}."
        val actual = configurator.decodeConfig(input)
        assertThat(actual.sourceDirs, contains(new Path("src") as IPath, new Path("src2") as IPath))
    }
    
    @Test
    def void outputPathShouldBeConfigured() {
        val input = "{erl_opts, []}."
        val actual = configurator.decodeConfig(input)
        assertThat(actual.outputDir, is(new Path("ebin")))
    }
}
