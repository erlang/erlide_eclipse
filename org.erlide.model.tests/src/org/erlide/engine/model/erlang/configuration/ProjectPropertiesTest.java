package org.erlide.engine.model.erlang.configuration;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.hasSize;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.util.ErlideTestUtils;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import com.google.common.collect.Lists;

public class ProjectPropertiesTest {
    private static IErlProject erlProject;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        ErlideTestUtils.initProjects();
        // We set up projects here, it's quite costly
        final String name1 = "testproject1";
        erlProject = ErlideTestUtils.createProject(ErlideTestUtils.getTmpPath(name1),
                name1);
    }

    @AfterClass
    public static void tearDownAfterClass() throws Exception {
        ErlideTestUtils.deleteProjects();
    }

    @Test
    public void setSourcesTest() {
        final IPath sd1 = new Path("src");
        assertThat(erlProject.getProperties().getSourceDirs(), contains(sd1));

        final IPath sd2 = new Path("a");
        erlProject.getProperties().setSourceDirs(Lists.newArrayList(sd2));
        assertThat(erlProject.getProperties().getSourceDirs(), hasSize(1));
        assertThat(erlProject.getProperties().getSourceDirs(), contains(sd2));
    }

}
