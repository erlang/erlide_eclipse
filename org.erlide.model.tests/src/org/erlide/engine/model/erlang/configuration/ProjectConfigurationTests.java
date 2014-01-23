package org.erlide.engine.model.erlang.configuration;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.root.ProjectConfigType;
import org.junit.Test;

public class ProjectConfigurationTests extends AbstractProjectConfigurationTests {

    @Test
    public void defaultProjectBuilderShouldBeInternal() {
        assertThat(project.getBuilderProperties().getBuilderTool(),
                is(BuilderTool.INTERNAL));
        assertThat(project.getBuilderConfigType(), is(ProjectConfigType.INTERNAL));
    }

    @Test
    public void canSetProjectConfig() {
        project.setBuilderConfigType(ProjectConfigType.INTERNAL);
    }

    @Override
    public void configCanBeParsed() {
        // not relevant
    }
}
