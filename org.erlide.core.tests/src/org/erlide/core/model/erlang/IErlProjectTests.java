package org.erlide.core.model.erlang;

import static org.junit.Assert.*;

import java.io.File;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.backend.runtimeinfo.RuntimeInfo;
import org.erlide.core.internal.model.root.OldErlangProjectProperties;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlProject;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.Test;

import com.ericsson.otp.erlang.RuntimeVersion;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

public class IErlProjectTests extends ErlModelTestBase {

    private static final String XX_ERLIDEX = "xx.erlidex";

    // Collection<IErlModule> getModules() throws ErlModelException;
    @Test
    public void getModules() throws Exception {
        ErlideTestUtils.createInclude(projects[0], "bb.erl", "-module(bb).\n");
        ErlideTestUtils.createModule(projects[0], "cc.hrl",
                "-define(A, hej).\n");
        ErlideTestUtils.createInclude(projects[0], "dd.hrl",
                "-define(B, du).\n");
        final List<IErlModule> expected = Lists.newArrayList(module);
        final Collection<IErlModule> modules = projects[0].getModules();
        assertEquals(expected, modules);
    }

    // FIXME write tests that gives exceptions!

    // Collection<IErlModule> getIncludes() throws ErlModelException;
    @Test
    public void getIncludes() throws Exception {
        ErlideTestUtils.createModule(projects[0], "aa.erl", "-module(aa).\n");
        ErlideTestUtils.createInclude(projects[0], "bb.erl", "-module(bb).\n");
        ErlideTestUtils.createModule(projects[0], "cc.hrl",
                "-define(A, hej).\n");
        final IErlModule includeDD = ErlideTestUtils.createInclude(projects[0],
                "dd.hrl", "-define(B, du).\n");
        final List<IErlModule> expected = Lists.newArrayList(includeDD);
        final Collection<IErlModule> includes = projects[0].getIncludes();
        assertEquals(expected, includes);
    }

    // Collection<IErlModule> getModulesAndIncludes() throws ErlModelException;
    @Test
    public void getModulesAndIncludes() throws Exception {
        ErlideTestUtils.createInclude(projects[0], "bb.erl", "-module(bb).\n");
        ErlideTestUtils.createModule(projects[0], "cc.hrl",
                "-define(A, hej).\n");
        final IErlModule includeD = ErlideTestUtils.createInclude(projects[0],
                "dd.hrl", "-define(B, du).\n");
        // FIXME should all of them be returned?
        final List<IErlModule> expected = Lists.newArrayList(module, includeD);
        final Collection<IErlModule> includes = projects[0]
                .getModulesAndIncludes();
        assertEquals(expected, includes);
    }

    // Collection<IErlModule> getExternalModules() throws ErlModelException;
    // void setExternalModulesFile(String absolutePath)
    // throws BackingStoreException;
    @Test
    public void getExternalModules() throws Exception {
        File externalFile = null;
        File externalsFile = null;
        final IErlProject aProject = projects[0];
        final String externalModulesString = aProject
                .getExternalModulesString();
        try {
            // given
            // an erlang project and an external file not in any project
            final String externalFileName = "external.erl";
            externalFile = ErlideTestUtils
                    .createTmpFile(externalFileName,
                            "-module(external).\nf([_ | _]=L ->\n    atom_to_list(L).\n");
            final String absolutePath = externalFile.getAbsolutePath();
            externalsFile = ErlideTestUtils.createTmpFile(XX_ERLIDEX,
                    absolutePath);
            aProject.open(null);
            final Collection<IErlModule> otpModules = aProject
                    .getExternalModules();
            aProject.setExternalModulesFile(externalsFile.getAbsolutePath());
            aProject.open(null);
            // when
            // fetching all external modules
            final Collection<IErlModule> externalModules = aProject
                    .getExternalModules();
            // then
            // the external file should be returned
            final Set<IErlModule> otpSet = Sets.newHashSet(otpModules);
            final Set<IErlModule> externalSet = Sets
                    .newHashSet(externalModules);
            final Set<IErlModule> difference = Sets.difference(externalSet,
                    otpSet);
            assertEquals(1, difference.size());
            final IErlModule externalModule = difference.iterator().next();
            assertNotNull(externalModule);
            assertEquals(absolutePath, externalModule.getFilePath());
        } finally {
            if (externalFile != null && externalFile.exists()) {
                externalFile.delete();
            }
            if (externalsFile != null && externalsFile.exists()) {
                externalsFile.delete();
            }
            aProject.setExternalModulesFile(externalModulesString);
        }
    }

    // Collection<IErlModule> getExternalIncludes() throws ErlModelException;
    // void setExternalIncludesFile(String absolutePath)
    // throws BackingStoreException;
    @Test
    public void getExternalIncludes() throws Exception {
        File externalFile = null;
        File externalsFile = null;
        final IErlProject aProject = projects[0];
        final String externalIncludesString = aProject
                .getExternalIncludesString();
        try {
            // given
            // an erlang project and an external file not in any project
            final String externalFileName = "external.hrl";
            externalFile = ErlideTestUtils.createTmpFile(externalFileName,
                    "-define(E, hej).\n");
            final String absolutePath = externalFile.getAbsolutePath();
            final String externalsFileName = XX_ERLIDEX;
            externalsFile = ErlideTestUtils.createTmpFile(externalsFileName,
                    absolutePath);
            aProject.open(null);
            final Collection<IErlModule> otpIncludes = aProject
                    .getExternalIncludes();
            aProject.setExternalIncludesFile(externalsFile.getAbsolutePath());
            aProject.open(null);
            // when
            // fetching all external includes
            final Collection<IErlModule> externalIncludes = aProject
                    .getExternalIncludes();
            // then
            // the external file should be returned
            final Set<IErlModule> otpSet = Sets.newHashSet(otpIncludes);
            final Set<IErlModule> externalSet = Sets
                    .newHashSet(externalIncludes);
            final Set<IErlModule> difference = Sets.difference(externalSet,
                    otpSet);
            final IErlModule externalInclude = difference.iterator().next();
            assertNotNull(externalInclude);
            assertEquals(absolutePath, externalInclude.getFilePath());
        } finally {
            if (externalFile != null && externalFile.exists()) {
                externalFile.delete();
            }
            if (externalsFile != null && externalsFile.exists()) {
                externalsFile.delete();
            }
            aProject.setExternalIncludesFile(externalIncludesString);
        }
    }

    @Test
    public void getExternalIncludes_includeDirs() throws Exception {
        File externalFile = null;
        final IErlProject aProject = projects[0];
        final Collection<IPath> includeDirs = aProject.getIncludeDirs();
        try {
            // given
            // an erlang project and an external file not in any project, but on
            // the include-path
            final String externalFileName = "external.hrl";
            externalFile = ErlideTestUtils.createTmpFile(externalFileName,
                    "-define(E, hej).\n");
            final String absolutePath = externalFile.getAbsolutePath();
            final List<IPath> newIncludeDirs = Lists.newArrayList(includeDirs);
            aProject.open(null);
            final Collection<IErlModule> otpIncludes = aProject
                    .getExternalIncludes();
            final IPath absoluteDir = new Path(absolutePath)
                    .removeLastSegments(1);
            newIncludeDirs.add(absoluteDir);
            aProject.setIncludeDirs(newIncludeDirs);
            aProject.open(null);
            // when
            // fetching all external includes
            final Collection<IErlModule> externalIncludes = aProject
                    .getExternalIncludes();
            // then
            // the external file should be returned
            final Set<IErlModule> otpSet = Sets.newHashSet(otpIncludes);
            final Set<IErlModule> externalSet = Sets
                    .newHashSet(externalIncludes);
            final Set<IErlModule> difference = Sets.difference(externalSet,
                    otpSet);
            final IErlModule externalInclude = difference.iterator().next();
            assertNotNull(externalInclude);
            assertEquals(absolutePath, externalInclude.getFilePath());
        } finally {
            if (externalFile != null && externalFile.exists()) {
                externalFile.delete();
            }
            aProject.setIncludeDirs(includeDirs);
        }
    }

    // String getExternalModulesString();
    @Test
    public void getExternalModulesString() throws Exception {
        final IErlProject aProject = projects[0];
        final String externalIncludesString = aProject
                .getExternalIncludesString();
        try {
            final String s = "/hej";
            aProject.setExternalModulesFile(s);
            assertEquals(s, aProject.getExternalModulesString());
        } finally {
            aProject.setExternalModulesFile(externalIncludesString);
        }
    }

    // String getExternalIncludesString();
    @Test
    public void getExternalIncludesString() throws Exception {
        final IErlProject aProject = projects[0];
        final String externalIncludesString = aProject
                .getExternalIncludesString();
        try {
            final String s = "/tjo";
            aProject.setExternalIncludesFile(s);
            assertEquals(s, aProject.getExternalIncludesString());
        } finally {
            aProject.setExternalIncludesFile(externalIncludesString);
        }
    }

    // void setIncludeDirs(Collection<IPath> includeDirs)
    // throws BackingStoreException;
    @Test
    public void setIncludeDirs() throws Exception {
        File externalFile = null;
        final IErlProject aProject = projects[0];
        final Collection<IPath> includeDirs = aProject.getIncludeDirs();
        try {
            // given
            // an erlang project and an external file not in any project
            final String externalFileName = "external.hrl";
            externalFile = ErlideTestUtils.createTmpFile(externalFileName,
                    "-define(E, hej).\n");
            final String absolutePath = externalFile.getAbsolutePath();
            final List<IPath> newIncludeDirs = Lists.newArrayList(includeDirs);
            aProject.open(null);
            final Collection<IErlModule> otpIncludes = aProject
                    .getExternalIncludes();
            final IPath absoluteDir = new Path(absolutePath)
                    .removeLastSegments(1);
            newIncludeDirs.add(absoluteDir);
            aProject.setIncludeDirs(newIncludeDirs);
            aProject.open(null);
            // when
            // fetching all external includes
            final Collection<IErlModule> externalIncludes = aProject
                    .getExternalIncludes();
            // then
            // the external file should be returned
            final Set<IErlModule> otpSet = Sets.newHashSet(otpIncludes);
            final Set<IErlModule> externalSet = Sets
                    .newHashSet(externalIncludes);
            final Set<IErlModule> difference = Sets.difference(externalSet,
                    otpSet);
            final IErlModule externalInclude = difference.iterator().next();
            assertNotNull(externalInclude);
            assertEquals(absolutePath, externalInclude.getFilePath());
        } finally {
            if (externalFile != null && externalFile.exists()) {
                externalFile.delete();
            }
            aProject.setIncludeDirs(includeDirs);
        }
    }

    // void setSourceDirs(Collection<IPath> sourceDirs)
    // throws BackingStoreException;
    @Test
    public void setSourceDirs() throws Exception {
        final IErlProject aProject = projects[0];
        final Collection<IPath> sourceDirs = aProject.getSourceDirs();
        try {
            // given
            // an Erlang project and a module
            final IPath srcxPath = new Path("srcx");
            final List<IPath> srcxDirs = Lists.newArrayList(srcxPath);
            aProject.open(null);
            // when
            // setting source dirs so the module is on source path
            final Collection<IErlModule> modules = aProject.getModules();
            aProject.setSourceDirs(srcxDirs);
            aProject.open(null);
            final Collection<IErlModule> srcxModules = aProject.getModules();
            aProject.setSourceDirs(sourceDirs);
            aProject.open(null);
            final Collection<IErlModule> modulesAgain = aProject.getModules();
            // then
            // the it should be returned, but not otherwise
            assertEquals(0, srcxModules.size());
            assertEquals(1, modules.size());
            assertEquals(module, modules.iterator().next());
            assertEquals(1, modulesAgain.size());
            assertEquals(module, modulesAgain.iterator().next());
        } finally {
            aProject.setSourceDirs(sourceDirs);
        }
    }

    // Collection<IPath> getSourceDirs();
    @Test
    public void getSourceDirs() throws Exception {
        final Collection<IPath> sourceDirs = projects[0].getSourceDirs();
        assertEquals(1, sourceDirs.size());
        final IPath path = new Path("src");
        assertEquals(path, sourceDirs.iterator().next());
    }

    // Collection<IPath> getIncludeDirs();
    @Test
    public void getIncludeDirs() throws Exception {
        final Collection<IPath> includeDirs = projects[0].getIncludeDirs();
        assertEquals(1, includeDirs.size());
        final IPath path = new Path("include");
        assertEquals(path, includeDirs.iterator().next());
    }

    // IPath getOutputLocation();
    @Test
    public void getOutputLocation() throws Exception {
        final IPath outputLocation = projects[0].getOutputLocation();
        assertEquals(new Path("ebin"), outputLocation);
    }

    // RuntimeInfo getRuntimeInfo();
    @Test
    public void getRuntimeInfo() throws Exception {
        final IErlProject aProject = projects[0];
        final RuntimeInfo info = aProject.getRuntimeInfo();
        // final String expected = ResourcesPlugin.getWorkspace().getRoot()
        // .getLocation().toString();
        assertNotNull(info);
        // The working dir might be relative to the project and can also be "."
        // We need to convert it to a canonical absolute path in order to be
        // able to compare it with a value.
        // This is not very portable across OSs
    }

    // RuntimeVersion getRuntimeVersion();
    @Test
    public void getRuntimeVersion() throws Exception {
        final IErlProject aProject = projects[0];
        final RuntimeVersion version = aProject.getRuntimeVersion();
        assertNotNull(version);
        final String major = version.asMajor().toString();
        assertTrue(major.startsWith("R"));
        final int majorVersion = Integer.valueOf(major.substring(1)).intValue();
        assertTrue(majorVersion >= 12);
    }

    // boolean hasSourceDir(IPath fullPath);
    @Test
    public void hasSourceDir() throws Exception {
        final IErlProject aProject = projects[0];
        final IPath dot = new Path(".");
        assertTrue(aProject.hasSourceDir(dot));
        final IPath projectPath = aProject.getWorkspaceProject().getFullPath();
        final IPath src = projectPath.append("src");
        assertTrue(aProject.hasSourceDir(src));
        final IPath srcx = projectPath.append("srcx");
        assertFalse(aProject.hasSourceDir(srcx));
        final IPath include = projectPath.append("include");
        assertFalse(aProject.hasSourceDir(include));
    }

    // TODO check more properties than source dirs property
    /**
     * @see org.erlide.core.model.root.IErlProject#setAllProperties()
     */
    @Test
    public void setAllProperties() throws Exception {
        final IErlProject aProject = projects[0];
        final Collection<IPath> sourceDirs = aProject.getSourceDirs();
        try {
            final OldErlangProjectProperties properties = new OldErlangProjectProperties(
                    aProject.getWorkspaceProject());
            final IPath srcx = new Path("srcx");
            properties.setSourceDirs(Lists.newArrayList(srcx));
            aProject.setAllProperties(properties);
            final Collection<IPath> sourceDirs2 = aProject.getSourceDirs();
            assertEquals(1, sourceDirs2.size());
            assertEquals(srcx, sourceDirs2.iterator().next());
        } finally {
            aProject.setSourceDirs(sourceDirs);
        }
    }

    /**
     * @see org.erlide.core.model.root.IErlProject#getReferencedProjects()
     */
    @Test
    public void getReferencedProjects() throws Exception {
        final IProject aProject = projects[0].getWorkspaceProject();
        final IProjectDescription description = aProject.getDescription();
        final IProject[] refs = new IProject[] { projects[1]
                .getWorkspaceProject() };
        try {
            description.setReferencedProjects(refs);
            aProject.setDescription(description, null);
            final List<IErlProject> expected = Lists.newArrayList(projects[1]);
            assertEquals(expected, projects[0].getReferencedProjects());
        } finally {
            description.setReferencedProjects(new IProject[0]);
            aProject.setDescription(description, null);
        }
    }

    @Test(expected = ErlModelException.class)
    public void getProjectReferences_closedProject() throws Exception {
        final IErlProject erlProject = projects[0];
        final IProject aProject = erlProject.getWorkspaceProject();
        try {
            aProject.close(null);
            erlProject.getReferencedProjects();
        } finally {
            if (!aProject.isOpen()) {
                aProject.open(null);
            }
        }
    }

    // IErlModule getModule(String name) throws ErlModelException;
    @Test
    public void getModule() throws Exception {
        final IErlProject aProject = projects[0];
        final Collection<IPath> sourceDirs = aProject.getSourceDirs();
        try {
            // given
            // an Erlang project and a module
            final IErlModule aModule = ErlideTestUtils.createModule(aProject,
                    "aa.erl", "-module(aa).\n");
            final IPath srcxPath = new Path("srcx");
            final List<IPath> srcxDirs = Lists.newArrayList(srcxPath);
            aProject.open(null);
            // when
            // setting source dirs so the module is on source path
            final IErlModule module2 = aProject.getModule("aa");
            final IErlModule nullModule = aProject.getModule("aa.hrl");
            final IErlModule nullModule2 = aProject.getModule("AA");
            final IErlModule nullModule3 = aProject.getModule("aA");
            final IErlModule nullModule4 = aProject.getModule("AA.erl");
            final IErlModule module4 = aProject.getModule("aa.erl");
            aProject.setSourceDirs(srcxDirs);
            aProject.open(null);
            final IErlModule srcxModule = aProject.getModule("aa");
            aProject.setSourceDirs(sourceDirs);
            aProject.open(null);
            final IErlModule module3 = aProject.getModule("aa");
            // then
            // the it should be returned, but not otherwise
            assertEquals(aModule, module2);
            assertNull(srcxModule);
            assertNull(nullModule);
            assertNull(nullModule2);
            assertNull(nullModule3);
            assertNull(nullModule4);
            assertEquals(aModule, module3);
            assertEquals(aModule, module4);
        } finally {
            aProject.setSourceDirs(sourceDirs);
        }
    }

    // IProject getWorkspaceProject();
    @Test
    public void getWorkspaceProject() throws Exception {
        final IErlProject aProject = projects[0];
        final IProject workspaceProject = aProject.getWorkspaceProject();
        assertNotNull(workspaceProject);
        assertEquals(aProject.getName(), workspaceProject.getName());
    }
}
