package org.erlide.engine.model.erlang;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.model.root.ProjectPreferencesConstants;
import org.erlide.engine.util.ErlideTestUtils;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import com.google.common.base.Joiner;
import com.google.common.base.Splitter;
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

    @Test
    public void outputPathShouldFollowPropertyChange() {
        final String expected = "hello/world";

        final IProject project = erlProject.getWorkspaceProject();
        final IEclipsePreferences node = new ProjectScope(project)
                .getNode("org.erlide.core");
        node.put(ProjectPreferencesConstants.OUTPUT_DIR, expected);

        final ErlangProjectProperties pp = erlProject.getProperties();
        final String actual = pp.getOutputDir().toPortableString();

        assertEquals(expected, actual);
    }

    @Test
    public void includePathsShouldFollowPropertyChange() {
        final String expected = "hello/world;a/b";

        final IProject project = erlProject.getWorkspaceProject();
        final IEclipsePreferences node = new ProjectScope(project)
                .getNode("org.erlide.core");
        node.put(ProjectPreferencesConstants.INCLUDE_DIRS, expected);

        final ErlangProjectProperties pp = erlProject.getProperties();
        final String actual = pp.getIncludeDirs().toString();

        assertEquals(convertListString(expected), actual);
    }

    private String convertListString(final String expected) {
        return "[" + Joiner.on(", ").join(Splitter.on(";").split(expected)) + "]";
    }

    @Test
    public void sourcePathsShouldFollowPropertyChange() {
        final String expected = "hello/world;a/b";

        final IProject project = erlProject.getWorkspaceProject();
        final IEclipsePreferences node = new ProjectScope(project)
                .getNode("org.erlide.core");
        node.put(ProjectPreferencesConstants.SOURCE_DIRS, expected);

        final ErlangProjectProperties pp = erlProject.getProperties();
        final String actual = pp.getSourceDirs().toString();

        assertThat(actual, is(convertListString(expected)));
    }

}
