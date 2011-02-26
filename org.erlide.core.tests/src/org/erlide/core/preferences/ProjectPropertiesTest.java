package org.erlide.core.preferences;

import static org.junit.Assert.assertEquals;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.model.erlang.IErlProject;
import org.erlide.core.model.erlang.internal.OldErlangProjectProperties;
import org.erlide.core.model.erlang.internal.ProjectPreferencesConstants;
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
        String expected = "hello/world";

        IProject project = erlProject.getProject();
        final IEclipsePreferences node = new ProjectScope(project)
                .getNode(ErlangPlugin.PLUGIN_ID);
        node.put(ProjectPreferencesConstants.OUTPUT_DIR, expected);

        OldErlangProjectProperties pp = new OldErlangProjectProperties(project);
        String actual = pp.getOutputDir().toPortableString();

        assertEquals(expected, actual);
    }

    @Test
    public void includePathsShouldFollowPropertyChange() {
        String expected = "hello/world;a/b";

        IProject project = erlProject.getProject();
        final IEclipsePreferences node = new ProjectScope(project)
                .getNode(ErlangPlugin.PLUGIN_ID);
        node.put(ProjectPreferencesConstants.INCLUDE_DIRS, expected);

        OldErlangProjectProperties pp = new OldErlangProjectProperties(project);
        String actual = pp.getIncludeDirs().toString();

        assertEquals(convertListString(expected), actual);
    }

    private String convertListString(String expected) {
        return "[" + Joiner.on(", ").join(Splitter.on(";").split(expected))
                + "]";
    }

    @Test
    public void sourcePathsShouldFollowPropertyChange() {
        String expected = "hello/world;a/b";

        IProject project = erlProject.getProject();
        final IEclipsePreferences node = new ProjectScope(project)
                .getNode(ErlangPlugin.PLUGIN_ID);
        node.put(ProjectPreferencesConstants.SOURCE_DIRS, expected);

        OldErlangProjectProperties pp = new OldErlangProjectProperties(project);
        String actual = pp.getSourceDirs().toString();

        assertEquals(convertListString(expected), actual);
    }
}
