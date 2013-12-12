package org.erlide.engine.model.erlang.configuration;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.erlide.engine.model.builder.BuilderConfigType;
import org.erlide.engine.model.builder.BuilderTool;
import org.junit.Test;

public class ProjectConfigurationTests extends AbstractProjectConfigurationTests {

    @Test
    public void defaultProjectBuilderShouldBeInternal() {
        assertThat(project.getBuilderProperties().getBuilderTool(),
                is(BuilderTool.INTERNAL));
        assertThat(project.getBuilderConfigType(), is(BuilderConfigType.INTERNAL));
    }

    @Test
    public void canSetProjectConfig() {
        project.setBuilderConfigType(BuilderConfigType.INTERNAL);
    }

    @Override
    public void configCanBeParsed() {
        // not relevant
    }
}
