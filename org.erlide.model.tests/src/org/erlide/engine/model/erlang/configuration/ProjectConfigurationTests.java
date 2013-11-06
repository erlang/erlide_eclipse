package org.erlide.engine.model.erlang.configuration;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.erlide.engine.internal.model.root.ErlProject;
import org.erlide.engine.model.builder.BuilderConfig;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.util.ErlideTestUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class ProjectConfigurationTests {

    private IErlProject project;

    @Before
    public void setUp() throws Exception {
        ErlideTestUtils.initProjects();
        final String name = "testproject3";
        project = ErlideTestUtils.createProject(ErlideTestUtils.getTmpPath(name), name);
        final ErlProject p = (ErlProject) project;
        p.loadCoreProperties();
    }

    @After
    public void tearDown() throws Exception {
        ErlideTestUtils.deleteProjects();
    }

    @Test
    public void defaultProjectBuilderShouldBeInternal() {
        assertThat(project.getBuilderTool(), is(BuilderTool.INTERNAL));
        assertThat(project.getBuilderConfig(), is(BuilderConfig.INTERNAL));
    }

    @Test
    public void canSetProjectConfig() {
        project.setBuilderConfig(BuilderConfig.INTERNAL);
    }

    @Test(expected = IllegalArgumentException.class)
    public void canNotSetIncompatibleConfig() {
        project.setBuilderConfig(BuilderConfig.EMAKE);
    }

    @Test
    public void canSetCompatibleConfig() {
        project.setBuilderTool(BuilderTool.EMAKE);
        project.setBuilderConfig(BuilderConfig.EMAKE);
    }

    @Test
    public void configFollowsTool() throws Exception {
        project.setBuilderTool(BuilderTool.EMAKE);
        assertThat(project.getBuilderConfig(), is(BuilderConfig.EMAKE));
        project.setBuilderTool(BuilderTool.REBAR);
        assertThat(project.getBuilderConfig(), is(BuilderConfig.REBAR));
        project.setBuilderTool(BuilderTool.INTERNAL);
        assertThat(project.getBuilderConfig(), is(BuilderConfig.INTERNAL));
        project.setBuilderTool(BuilderTool.MAKE);
        assertThat(project.getBuilderConfig(), is(BuilderConfig.INTERNAL));
    }
}
