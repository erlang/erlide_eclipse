package org.erlide.core.model.erlang;

import static org.junit.Assert.*;

import java.util.Collection;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.core.model.root.IErlFolder;
import org.erlide.core.model.root.IErlProject;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.google.common.collect.Lists;

public class IErlFolderTests extends ErlModelTestBase {

    private IErlFolder srcFolder;
    private IErlFolder includeFolder;
    private IErlFolder ebinFolder;
    private static IErlProject project2;

    @Before
    @Override
    public void setUp() throws Exception {
        super.setUp();
        srcFolder = (IErlFolder) project.getChildNamed("src");
        includeFolder = (IErlFolder) project.getChildNamed("include");
        ebinFolder = (IErlFolder) project.getChildNamed("ebin");
    }

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        ErlModelTestBase.setUpBeforeClass();
        project2 = projects[1];
        final IProject workspaceProject = project2.getWorkspaceProject();
        final String src = "src";
        final IFolder folder = workspaceProject.getFolder(src);
        final String below = "below";
        final IFolder folder2 = folder.getFolder(below);
        ErlideTestUtils.createFolderHelper(folder2);
        final IPath belowPath = new Path(src).append(below);
        final Collection<IPath> sourceDirs = Lists.newArrayList(belowPath);
        project2.setSourceDirs(sourceDirs);
    }

    // Collection<IErlModule> getModules() throws ErlModelException;
    @Test
    public void getModules() throws Exception {
        final Collection<IErlModule> modules = srcFolder.getModules();
        final Collection<IErlModule> modules2 = includeFolder.getModules();
        assertEquals(1, modules.size());
        assertEquals(0, modules2.size());
    }

    // boolean isOnSourcePath();
    @Test
    public void isOnSourcePath() throws Exception {
        assertTrue(srcFolder.isOnSourcePath());
        assertFalse(includeFolder.isOnSourcePath());
        assertFalse(ebinFolder.isOnSourcePath());
    }

    // boolean isOnIncludePath();
    @Test
    public void isOnIncludePath() throws Exception {
        assertFalse(srcFolder.isOnIncludePath());
        assertTrue(includeFolder.isOnIncludePath());
        assertFalse(ebinFolder.isOnIncludePath());
    }

    // boolean isSourcePathParent();
    @Test
    public void isSourcePathParent() throws Exception {
        final IErlFolder srcFolder2 = (IErlFolder) project2
                .getChildNamed("src");
        final IErlFolder includeFolder2 = (IErlFolder) project2
                .getChildNamed("include");
        final IErlFolder ebinFolder2 = (IErlFolder) project2
                .getChildNamed("ebin");
        assertTrue(srcFolder2.isSourcePathParent());
        assertFalse(includeFolder2.isSourcePathParent());
        assertFalse(ebinFolder2.isSourcePathParent());
    }

    // IErlModule findModule(String moduleName, String modulePath)
    // throws ErlModelException;
    @Test
    public void findModule() throws Exception {
        final String moduleName = module.getModuleName();
        final String name = module.getName();
        final String filePath = module.getFilePath();
        final IErlModule include = ErlideTestUtils.createInclude(project,
                "yy.hrl", "-define(ME, yy).\n");
        final IErlModule findModule = srcFolder.findModule(moduleName, null);
        final IErlModule findModule2 = srcFolder.findModule(null, filePath);
        final IErlModule findModule3 = includeFolder.findModule(moduleName,
                null);
        final IErlModule findModule4 = includeFolder.findModule(null, filePath);
        final IErlModule findModule5 = srcFolder.findModule(name, null);
        // path overrides name
        final IErlModule findModule6 = srcFolder.findModule("xxaa", filePath);
        final IErlModule findModule7 = includeFolder.findModule("yy.hrl", null);
        final IErlModule findModule8 = includeFolder.findModule(null,
                include.getFilePath());
        final IErlModule findModule9 = includeFolder.findModule("yy", null);
        assertEquals(module, findModule);
        assertEquals(module, findModule2);
        assertNull(findModule3);
        assertNull(findModule4);
        assertEquals(module, findModule5);
        assertEquals(module, findModule6);
        assertEquals(include, findModule7);
        assertEquals(include, findModule8);
        assertNull(findModule9);
    }

    // IErlModule findInclude(String includeName, String includePath)
    // throws ErlModelException;
    @Test
    public void findInclude() throws Exception {
        final IErlModule include = ErlideTestUtils.createInclude(project,
                "yy.hrl", "-define(ME, yy).\n");
        final IErlModule module2 = ErlideTestUtils.createInclude(project,
                "zz.erl", "-module(zz).\n");
        final String moduleName = include.getModuleName();
        final String name = include.getName();
        final String filePath = include.getFilePath();
        final IErlModule findInclude = srcFolder.findInclude(moduleName, null);
        final IErlModule findInclude2 = includeFolder.findInclude(moduleName,
                null);
        final IErlModule findInclude3 = includeFolder.findInclude(null,
                filePath);
        final IErlModule findInclude4 = includeFolder.findInclude(name, null);
        final IErlModule findInclude5 = includeFolder.findInclude("xxaa",
                filePath);
        final IErlModule findInclude6 = includeFolder.findInclude("zz.erl",
                null);
        final IErlModule findInclude7 = includeFolder.findInclude("zz", null);
        assertNull(findInclude);
        assertEquals(include, findInclude2);
        assertEquals(include, findInclude3);
        assertEquals(include, findInclude4);
        assertEquals(include, findInclude5);
        assertEquals(module2, findInclude6);
        assertNull(findInclude7);
    }

}
