package org.erlide.engine.model.erlang.configuration;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.erlide.engine.model.builder.BuilderConfig;
import org.erlide.engine.model.builder.BuilderTool;
import org.junit.Test;

public class ProjectConfigurationTests extends AbstractProjectConfigurationTests {

    @Test
    public void defaultProjectBuilderShouldBeInternal() {
        assertThat(project.getBuilderProperties().getBuilderTool(),
                is(BuilderTool.INTERNAL));
        assertThat(project.getBuilderConfig(), is(BuilderConfig.INTERNAL));
    }

    @Test
    public void canSetProjectConfig() {
        project.setBuilderConfig(BuilderConfig.INTERNAL);
    }

    @Test(expected = IllegalArgumentException.class)
    public void canNotSetIncompatibleConfig() {
        project.getBuilderProperties().setBuilderTool(BuilderTool.EMAKE);
        project.setBuilderConfig(BuilderConfig.REBAR);
    }

    @Test
    public void canSetCompatibleConfig() {
        project.getBuilderProperties().setBuilderTool(BuilderTool.EMAKE);
        project.setBuilderConfig(BuilderConfig.EMAKE);
    }

    @Test
    public void configFollowsTool() throws Exception {
        project.getBuilderProperties().setBuilderTool(BuilderTool.EMAKE);
        assertThat(project.getBuilderConfig(), is(BuilderConfig.EMAKE));
        project.getBuilderProperties().setBuilderTool(BuilderTool.REBAR);
        assertThat(project.getBuilderConfig(), is(BuilderConfig.REBAR));
        project.getBuilderProperties().setBuilderTool(BuilderTool.INTERNAL);
        assertThat(project.getBuilderConfig(), is(BuilderConfig.REBAR));
        project.getBuilderProperties().setBuilderTool(BuilderTool.MAKE);
        assertThat(project.getBuilderConfig(), is(BuilderConfig.REBAR));
    }

    @Override
    public void configCanBeParsed() {
        // not relevant
    }
}
