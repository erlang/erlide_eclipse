package org.erlide.engine.model.erlang.configuration;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.erlide.engine.model.builder.BuilderConfig;
import org.erlide.engine.model.builder.BuilderTool;
import org.junit.Test;

public class ProjectConfigurationTests extends AbstractProjectConfigurationTests {

    @Test
    public void defaultProjectBuilderShouldBeInternal() {
        assertThat(props.getBuilderTool(), is(BuilderTool.INTERNAL));
        assertThat(props.getBuilderConfig(), is(BuilderConfig.INTERNAL));
    }

    @Test
    public void canSetProjectConfig() {
        props.setBuilderConfig(BuilderConfig.INTERNAL);
    }

    @Test(expected = IllegalArgumentException.class)
    public void canNotSetIncompatibleConfig() {
        props.setBuilderTool(BuilderTool.EMAKE);
        props.setBuilderConfig(BuilderConfig.REBAR);
    }

    @Test
    public void canSetCompatibleConfig() {
        props.setBuilderTool(BuilderTool.EMAKE);
        props.setBuilderConfig(BuilderConfig.EMAKE);
    }

    @Test
    public void configFollowsTool() throws Exception {
        props.setBuilderTool(BuilderTool.EMAKE);
        assertThat(props.getBuilderConfig(), is(BuilderConfig.EMAKE));
        props.setBuilderTool(BuilderTool.REBAR);
        assertThat(props.getBuilderConfig(), is(BuilderConfig.REBAR));
        props.setBuilderTool(BuilderTool.INTERNAL);
        assertThat(props.getBuilderConfig(), is(BuilderConfig.REBAR));
        props.setBuilderTool(BuilderTool.MAKE);
        assertThat(props.getBuilderConfig(), is(BuilderConfig.REBAR));
    }

    @Override
    public void configCanBeParsed() {
        // not relevant
    }
}
