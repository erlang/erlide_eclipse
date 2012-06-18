package org.erlide.core.util;

import static org.junit.Assert.*;

import java.util.Collection;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlProject;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.Before;
import org.junit.Test;

public class ModuleLocatorTests {

    private IErlProject p1;
    private IErlProject p2;

    @Before
    public void setup() throws ErlModelException {
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
        assertNotNull(p1);
        assertNotNull(p2);
    }

    @Test
    public void demoProjectsShouldBeConfiguredProperly()
            throws ErlModelException {
        checkProjectDirectories(p1, new IPath[] { new Path("src") },
                new IPath[] { new Path("include"),
                        new Path("T_ROOT/p3/include") });
        checkProjectDirectories(p2, new IPath[] { new Path("src") },
                new IPath[] { new Path("include") });
    }

    private void checkProjectDirectories(final IErlProject project,
            final Object[] expected_sources, final Object[] expected_includes)
            throws ErlModelException {
        assertArrayEquals(expected_sources, project.getSourceDirs().toArray());
        assertArrayEquals(expected_includes, project.getIncludeDirs().toArray());
        assertEquals("../external_modules", project.getExternalModulesString());
        assertEquals("../external_includes",
                project.getExternalIncludesString());
    }

    // @Test
    public void demoProjectsShouldHaveRightExternalModules()
            throws ErlModelException {
        checkExternalModules(p1, new String[] { "m11.erl" });
        checkExternalModules(p2, new String[] { "m11.erl" });
    }

    private void checkExternalModules(final IErlProject project,
            final String[] extmods) throws ErlModelException {
        checkModuleNamesInList(extmods, project.getExternalModules());
    }

    private void checkExternalIncludes(final IErlProject project,
            final String[] extincs) throws ErlModelException {
        checkModuleNamesInList(extincs, project.getExternalIncludes());
    }

    private void checkModuleNamesInList(final String[] mods,
            final Collection<IErlModule> list) {
        for (final String name : mods) {
            assertTrue(name, hasModWithName(list, name));
        }
    }

    private boolean hasModWithName(final Collection<IErlModule> list,
            final String name) {
        for (final IErlModule m : list) {
            if (m.getName().equals(name)) {
                return true;
            }
        }
        return false;
    }
}
