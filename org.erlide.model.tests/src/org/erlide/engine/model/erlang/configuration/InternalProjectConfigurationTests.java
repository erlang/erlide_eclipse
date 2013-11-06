package org.erlide.engine.model.erlang.configuration;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
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

public class InternalProjectConfigurationTests {
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
