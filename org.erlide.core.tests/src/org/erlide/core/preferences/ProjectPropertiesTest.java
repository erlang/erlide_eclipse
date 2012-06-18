package org.erlide.core.preferences;

import static org.junit.Assert.*;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.erlide.core.ErlangCore;
import org.erlide.core.internal.model.root.OldErlangProjectProperties;
import org.erlide.core.internal.model.root.ProjectPreferencesConstants;
import org.erlide.core.model.root.IErlProject;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import com.google.common.base.Joiner;
import com.google.common.base.Splitter;

public class ProjectPropertiesTest {
    private static IErlProject erlProject;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        ErlideTestUtils.initProjects();
        // We set up projects here, it's quite costly
        final String name1 = "testproject1";
        erlProject = ErlideTestUtils.createProject(
                ErlideTestUtils.getTmpPath(name1), name1);
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
                .getNode(ErlangCore.PLUGIN_ID);
        node.put(ProjectPreferencesConstants.OUTPUT_DIR, expected);

        final OldErlangProjectProperties pp = new OldErlangProjectProperties(
                project);
        final String actual = pp.getOutputDir().toPortableString();

        assertEquals(expected, actual);
    }

    @Test
    public void includePathsShouldFollowPropertyChange() {
        final String expected = "hello/world;a/b";

        final IProject project = erlProject.getWorkspaceProject();
        final IEclipsePreferences node = new ProjectScope(project)
                .getNode(ErlangCore.PLUGIN_ID);
        node.put(ProjectPreferencesConstants.INCLUDE_DIRS, expected);

        final OldErlangProjectProperties pp = new OldErlangProjectProperties(
                project);
        final String actual = pp.getIncludeDirs().toString();

        assertEquals(convertListString(expected), actual);
    }

    private String convertListString(final String expected) {
        return "[" + Joiner.on(", ").join(Splitter.on(";").split(expected))
                + "]";
    }

    @Test
    public void sourcePathsShouldFollowPropertyChange() {
        final String expected = "hello/world;a/b";

        final IProject project = erlProject.getWorkspaceProject();
        final IEclipsePreferences node = new ProjectScope(project)
                .getNode(ErlangCore.PLUGIN_ID);
        node.put(ProjectPreferencesConstants.SOURCE_DIRS, expected);

        final OldErlangProjectProperties pp = new OldErlangProjectProperties(
                project);
        final String actual = pp.getSourceDirs().toString();

        assertEquals(convertListString(expected), actual);
    }
}
