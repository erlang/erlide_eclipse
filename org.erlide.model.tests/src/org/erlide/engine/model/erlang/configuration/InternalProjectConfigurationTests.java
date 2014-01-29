package org.erlide.engine.model.erlang.configuration;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.ProjectPreferencesConstants;
import org.junit.Test;
import org.osgi.service.prefs.BackingStoreException;

import com.google.common.base.Joiner;
import com.google.common.base.Splitter;

public class InternalProjectConfigurationTests extends AbstractProjectConfigurationTests {

    @Test
    public void outputPathShouldFollowPropertyChange() throws BackingStoreException {
        final String expected = "hello/world";

        final IProject wproject = project.getWorkspaceProject();
        final IEclipsePreferences node = new ProjectScope(wproject)
                .getNode("org.erlide.core");
        node.put(ProjectPreferencesConstants.OUTPUT_DIR, expected);
        node.flush();

        final ErlangProjectProperties pp = project.getProperties();
        final String actual = pp.getOutputDir().toPortableString();

        assertThat(actual, is(expected));
    }

    @Test
    public void includePathsShouldFollowPropertyChange() throws BackingStoreException {
        final String expected = "hello/world;a/b";

        final IProject wproject = project.getWorkspaceProject();
        final IEclipsePreferences node = new ProjectScope(wproject)
                .getNode("org.erlide.core");
        node.put(ProjectPreferencesConstants.INCLUDE_DIRS, expected);
        node.flush();

        final String actual = project.getProperties().getIncludeDirs().toString();

        assertThat(actual, is(convertListString(expected)));
    }

    private String convertListString(final String expected) {
        return "[" + Joiner.on(", ").join(Splitter.on(";").split(expected)) + "]";
    }

    @Test
    public void sourcePathsShouldFollowPropertyChange() throws BackingStoreException {
        final String expected = "hello/world;a/b";

        final IProject wproject = project.getWorkspaceProject();
        final IEclipsePreferences node = new ProjectScope(wproject)
                .getNode("org.erlide.core");
        node.put(ProjectPreferencesConstants.SOURCE_DIRS, expected);
        node.flush();

        final String actual = project.getProperties().getSourceDirs().toString();

        assertThat(actual, is(convertListString(expected)));
    }

    @Override
    public void configCanBeParsed() {
        // not relevant
    }

}
