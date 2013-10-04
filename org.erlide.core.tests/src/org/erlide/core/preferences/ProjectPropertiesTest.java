package org.erlide.core.preferences;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;

import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.model.root.IErlangProjectProperties;
import org.erlide.engine.model.root.OldErlangProjectProperties;
import org.erlide.engine.model.root.ProjectPreferencesConstants;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.osgi.service.prefs.BackingStoreException;

import com.google.common.base.Joiner;
import com.google.common.base.Splitter;
import com.google.common.collect.Maps;

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
                .getNode("org.erlide.core");
        node.put(ProjectPreferencesConstants.OUTPUT_DIR, expected);

        final IErlangProjectProperties pp = new OldErlangProjectProperties(
                project);
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

        final IErlangProjectProperties pp = new OldErlangProjectProperties(
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
                .getNode("org.erlide.core");
        node.put(ProjectPreferencesConstants.SOURCE_DIRS, expected);

        final IErlangProjectProperties pp = new OldErlangProjectProperties(
                project);
        final String actual = pp.getSourceDirs().toString();

        assertThat(actual, is(convertListString(expected)));
    }

    @Test
    public void builderPropertiesShouldBeSavedAndRetrieved()
            throws BackingStoreException {
        final Map<String, String> expected = Maps.newHashMap();
        expected.put("builder", "make");
        expected.put("workdir", "/some/dir");
        expected.put("args", " -f hoo \"baz\" ");

        final IProject project = erlProject.getWorkspaceProject();
        IErlangProjectProperties pp = new OldErlangProjectProperties(project);
        pp.setBuilderProperties(expected);
        pp.store();
        pp = new OldErlangProjectProperties(project);
        final Map<String, String> actual = pp.getBuilderProperties();

        assertThat(actual.get("builder"), is(expected.get("builder")));
        assertThat(actual.get("workdir"), is(expected.get("workdir")));
        assertThat(actual.get("args"), is(expected.get("args")));
    }
}
