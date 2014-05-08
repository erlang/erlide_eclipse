package org.erlide.engine.util;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;

import java.util.Collection;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlProject;
import org.junit.Before;
import org.junit.Test;

public class ModuleLocatorTests {

    private IErlProject p1;
    private IErlProject p2;

    @Before
    public void setup() throws CoreException {
        p1 = ErlideTestUtils.getExistingProject("p1");
        p2 = ErlideTestUtils.getExistingProject("p2");
        if (p1 != null) {
            p1.makeConsistent(null);
        }
        if (p1 != null) {
            p2.makeConsistent(null);
        }
    }

    @Test
    public void demoProjectsShouldBeInWorkspace() {
        assertThat(p1, is(not(nullValue())));
        assertThat(p2, is(not(nullValue())));
    }

    @Test
    public void demoProjectsShouldBeConfiguredProperly() {
        checkProjectDirectories(p1, new IPath[] { new Path("src") }, new IPath[] {
                new Path("include"), new Path("T_ROOT/p3/include") });
        checkProjectDirectories(p2, new IPath[] { new Path("src") },
                new IPath[] { new Path("include") });
    }

    private void checkProjectDirectories(final IErlProject project,
            final Object[] expectedSources, final Object[] expectedIncludes) {
        final ErlangProjectProperties properties = project.getProperties();
        assertThat(properties.getSourceDirs().toArray(), is(expectedSources));
        assertThat(properties.getIncludeDirs().toArray(), is(expectedIncludes));
        assertThat(properties.getExternalModules(), is("../external_modules"));
        assertThat(properties.getExternalIncludes(), is("../external_includes"));
    }

    // @Test
    public void demoProjectsShouldHaveRightExternalModules() throws ErlModelException {
        checkExternalModules(p1, new String[] { "m11.erl" });
        checkExternalModules(p2, new String[] { "m11.erl" });
    }

    private void checkExternalModules(final IErlProject project, final String[] extmods)
            throws ErlModelException {
        checkModuleNamesInList(extmods, project.getExternalModules());
    }

    @SuppressWarnings("unused")
    private void checkExternalIncludes(final IErlProject project, final String[] extincs)
            throws ErlModelException {
        checkModuleNamesInList(extincs, project.getExternalIncludes());
    }

    private void checkModuleNamesInList(final String[] mods,
            final Collection<IErlModule> list) {
        for (final String name : mods) {
            assertThat(name, hasModWithName(list, name));
        }
    }

    private boolean hasModWithName(final Collection<IErlModule> list, final String name) {
        for (final IErlModule m : list) {
            if (m.getName().equals(name)) {
                return true;
            }
        }
        return false;
    }
}
