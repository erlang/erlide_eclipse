package org.erlide.engine.model.erlang.configuration;

import static com.google.common.truth.Truth.assertThat;

import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.root.ProjectConfigType;
import org.junit.Test;

public class ProjectConfigurationTest extends AbstractProjectConfigurationTest {

    @Test
    public void defaultProjectBuilderShouldBeInternal() {
        assertThat(project.getBuilderProperties().getBuilderTool())
                .isEqualTo(BuilderTool.INTERNAL);
        assertThat(project.getConfigType()).isEqualTo(ProjectConfigType.INTERNAL);
    }

    @Test
    public void canSetProjectConfig() {
        project.setConfigType(ProjectConfigType.INTERNAL);
    }

    @Override
    public void configCanBeParsed() {
        // not relevant
    }
}
