package org.erlide.core.model.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.core.backend.runtimeinfo.RuntimeInfo;
import org.erlide.core.model.erlang.IErlProject.Scope;
import org.erlide.core.model.erlang.internal.OldErlangProjectProperties;
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
        final String externalModulesString = aProject.getExternalModulesString();
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
        final String expected = ResourcesPlugin.getWorkspace().getRoot()
                .getLocation().toString();
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
     * @see org.erlide.core.model.erlang.IErlProject#setAllProperties()
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
     * @see org.erlide.core.model.erlang.IErlProject#getReferencedProjects()
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

    // enum Scope {
    // PROJECT_ONLY, REFERENCED_PROJECTS, ALL_PROJECTS
    // }
    // IErlModule findModule(String moduleName, String modulePath, Scope scope)
    // throws ErlModelException;
    @Test
    public void findModule() throws Exception {
        File externalModuleFile = null;
        File externalsFile = null;
        final IErlProject aProject = projects[0];
        final IProject workspaceProject = aProject.getWorkspaceProject();
        final IProject[] referencedProjects = workspaceProject
                .getReferencedProjects();
        final String externalModulesString = aProject.getExternalModulesString();
        // given
        // a project with an external module and an internal module and a
        // referenced project with a module
        try {
            final String xxErl = "xx.erl";
            externalModuleFile = ErlideTestUtils.createTmpFile(xxErl,
                    "-module(xx).\n");
            final String externalModulePath = externalModuleFile
                    .getAbsolutePath();
            externalsFile = ErlideTestUtils.createTmpFile(XX_ERLIDEX,
                    externalModulePath);
            aProject.setExternalModulesFile(externalsFile.getAbsolutePath());
            final IErlModule aModule = ErlideTestUtils.createModule(aProject,
                    "yy.erl", "-module(yy).\n");
            final IErlProject project1 = projects[1];
            final IErlModule referencedModule = ErlideTestUtils.createModule(
                    project1, "zz.erl", "-module(zz).\n");
            aProject.open(null);
            // when
            // looking for modules
            final String xx = "xx";
            final IErlModule x1 = aProject.findModule(xx, null,
                    Scope.PROJECT_ONLY);
            final IErlModule x2 = aProject.findModule(xx, null,
                    Scope.ALL_PROJECTS);
            final IErlModule x3 = aProject.findModule(xx, null,
                    Scope.REFERENCED_PROJECTS);
            final String yy = "yy";
            final IErlModule y1 = aProject.findModule(yy, null,
                    Scope.PROJECT_ONLY);
            final IErlModule y2 = aProject.findModule(yy, null,
                    Scope.ALL_PROJECTS);
            final IErlModule y3 = aProject.findModule(yy, null,
                    Scope.REFERENCED_PROJECTS);
            final IErlModule y4 = project1.findModule(yy, null,
                    Scope.PROJECT_ONLY);
            final IErlModule y5 = project1.findModule(yy, null,
                    Scope.ALL_PROJECTS);
            final IErlModule y6 = project1.findModule(yy, null,
                    Scope.REFERENCED_PROJECTS);
            final String zz = "zz";
            final IErlModule z1 = aProject.findModule(zz, null,
                    Scope.PROJECT_ONLY);
            final IErlModule z2 = aProject.findModule(zz, null,
                    Scope.ALL_PROJECTS);
            final IErlModule z3 = aProject.findModule(zz, null,
                    Scope.REFERENCED_PROJECTS);
            final IProjectDescription description = workspaceProject
                    .getDescription();
            description.setReferencedProjects(new IProject[] { project1
                    .getWorkspaceProject() });
            workspaceProject.setDescription(description, null);
            aProject.open(null);
            final IErlModule z4 = aProject.findModule(zz, null,
                    Scope.PROJECT_ONLY);
            final IErlModule z5 = aProject.findModule(zz, null,
                    Scope.ALL_PROJECTS);
            final IErlModule z6 = aProject.findModule(zz, null,
                    Scope.REFERENCED_PROJECTS);
            // then
            // scope should be respected
            assertNotNull(x1);
            assertEquals(xxErl, x1.getName());
            assertNotNull(x2);
            assertEquals(xxErl, x2.getName());
            assertNotNull(x3);
            assertEquals(xxErl, x3.getName());
            assertEquals(aModule, y1);
            assertEquals(aModule, y2);
            assertEquals(aModule, y3);
            assertNull(y4);
            assertEquals(aModule, y5);
            assertNull(y6);
            assertNull(z1);
            assertEquals(referencedModule, z2);
            assertNull(z3);
            assertNull(z4);
            assertEquals(referencedModule, z5);
            assertEquals(referencedModule, z6);
        } finally {
            if (externalModuleFile != null && externalModuleFile.exists()) {
                externalModuleFile.delete();
            }
            if (externalsFile != null && externalsFile.exists()) {
                externalsFile.delete();
            }
            aProject.setExternalModulesFile(externalModulesString);
            final IProjectDescription description = workspaceProject
                    .getDescription();
            description.setReferencedProjects(referencedProjects);
            workspaceProject.setDescription(description, null);
        }
    }

    @Test
    public void findModule_preferProjectFile() throws Exception {
        File externalModuleFile = null;
        File externalsFile = null;
        final IErlProject aProject = projects[0];
        final IProject workspaceProject = aProject.getWorkspaceProject();
        final IProject[] referencedProjects = workspaceProject
                .getReferencedProjects();
        final String externalModulesString = aProject.getExternalModulesString();
        // given
        // a project with an external include and a
        // referenced project with an include, both have same name
        try {
            final String zzErl = "zz.erl";
            final String xxxContents = "-module(zz).\n";
            externalModuleFile = ErlideTestUtils.createTmpFile(zzErl,
                    xxxContents);
            final String externalModulePath = externalModuleFile
                    .getAbsolutePath();
            externalsFile = ErlideTestUtils.createTmpFile(XX_ERLIDEX,
                    externalModulePath);
            aProject.setExternalModulesFile(externalsFile.getAbsolutePath());
            final IErlProject project1 = projects[1];
            final IErlModule referencedModule = ErlideTestUtils.createModule(
                    project1, zzErl, xxxContents);
            aProject.open(null);
            // when
            // looking for module
            final String zz = "zz";
            final IErlModule zz1 = aProject.findModule(zz, null,
                    Scope.PROJECT_ONLY);
            final IErlModule zz2 = aProject.findModule(zz, null,
                    Scope.ALL_PROJECTS);
            final IErlModule zz3 = aProject.findModule(zz, null,
                    Scope.REFERENCED_PROJECTS);
            final IProjectDescription description = workspaceProject
                    .getDescription();
            description.setReferencedProjects(new IProject[] { project1
                    .getWorkspaceProject() });
            workspaceProject.setDescription(description, null);
            aProject.open(null);
            final IErlModule zz4 = aProject.findModule(zz, null,
                    Scope.PROJECT_ONLY);
            final IErlModule zz5 = aProject.findModule(zz, null,
                    Scope.ALL_PROJECTS);
            final IErlModule zz6 = aProject.findModule(zz, null,
                    Scope.REFERENCED_PROJECTS);
            // then
            // the non-external should be preferred
            assertNotNull(zz1);
            assertEquals(zzErl, zz1.getName());
            assertNotSame(referencedModule, zz1);
            assertEquals(referencedModule, zz2);
            assertNotNull(zz3);
            assertEquals(zzErl, zz3.getName());
            assertNotSame(referencedModule, zz3);
            assertNotNull(zz4);
            assertNotSame(referencedModule, zz4);
            assertEquals(referencedModule, zz5);
            assertEquals(referencedModule, zz6);
        } finally {
            if (externalModuleFile != null && externalModuleFile.exists()) {
                externalModuleFile.delete();
            }
            if (externalsFile != null && externalsFile.exists()) {
                externalsFile.delete();
            }
            final IProjectDescription description = workspaceProject
                    .getDescription();
            description.setReferencedProjects(referencedProjects);
            workspaceProject.setDescription(description, null);
            aProject.setExternalModulesFile(externalModulesString);
        }
    }

    // IErlModule findInclude(String includeName, String includePath, Scope
    // scope)
    // throws ErlModelException;
    @Test
    public void findInclude() throws Exception {
        File externalIncludeFile = null;
        final IErlProject aProject = projects[0];
        final IProject workspaceProject = aProject.getWorkspaceProject();
        final IProject[] referencedProjects = workspaceProject
                .getReferencedProjects();
        final Collection<IPath> includeDirs = aProject.getIncludeDirs();
        // given
        // a project with an external include and an internal include and a
        // referenced project with an include
        try {
            final String xxHrl = "xx.hrl";
            externalIncludeFile = ErlideTestUtils.createTmpFile(xxHrl,
                    "-record(rec2, {field, another=def}.");
            final String externalIncludePath = externalIncludeFile
                    .getAbsolutePath();
            final IPath p = new Path(externalIncludePath).removeLastSegments(1);
            final List<IPath> newIncludeDirs = Lists.newArrayList(includeDirs);
            newIncludeDirs.add(p);
            aProject.setIncludeDirs(newIncludeDirs);
            final IErlModule include = ErlideTestUtils.createInclude(aProject,
                    "yy.hrl", "-define(Y, include).\n");
            final IErlProject project1 = projects[1];
            final IErlModule referencedInclude = ErlideTestUtils.createInclude(
                    project1, "zz.hrl", "-define(Z, referenced).\n");
            aProject.open(null);
            // when
            // looking for includes
            final String xx = "xx";
            final IErlModule x1 = aProject.findInclude(xx, null,
                    Scope.PROJECT_ONLY);
            final IErlModule x2 = aProject.findInclude(xx, null,
                    Scope.ALL_PROJECTS);
            final IErlModule x3 = aProject.findInclude(xx, null,
                    Scope.REFERENCED_PROJECTS);
            final String yy = "yy";
            final IErlModule y1 = aProject.findInclude(yy, null,
                    Scope.PROJECT_ONLY);
            final IErlModule y2 = aProject.findInclude(yy, null,
                    Scope.ALL_PROJECTS);
            final IErlModule y3 = aProject.findInclude(yy, null,
                    Scope.REFERENCED_PROJECTS);
            final IErlModule y4 = project1.findInclude(yy, null,
                    Scope.PROJECT_ONLY);
            final IErlModule y5 = project1.findInclude(yy, null,
                    Scope.ALL_PROJECTS);
            final IErlModule y6 = project1.findInclude(yy, null,
                    Scope.REFERENCED_PROJECTS);
            final String zz = "zz";
            final IErlModule z1 = aProject.findInclude(zz, null,
                    Scope.PROJECT_ONLY);
            final IErlModule z2 = aProject.findInclude(zz, null,
                    Scope.ALL_PROJECTS);
            final IErlModule z3 = aProject.findInclude(zz, null,
                    Scope.REFERENCED_PROJECTS);
            final IProjectDescription description = workspaceProject
                    .getDescription();
            description.setReferencedProjects(new IProject[] { project1
                    .getWorkspaceProject() });
            workspaceProject.setDescription(description, null);
            aProject.open(null);
            final IErlModule z4 = aProject.findInclude(zz, null,
                    Scope.PROJECT_ONLY);
            final IErlModule z5 = aProject.findInclude(zz, null,
                    Scope.ALL_PROJECTS);
            final IErlModule z6 = aProject.findInclude(zz, null,
                    Scope.REFERENCED_PROJECTS);
            // then
            // scope should be respected
            assertNotNull(x1);
            assertEquals(xxHrl, x1.getName());
            assertNotNull(x2);
            assertEquals(xxHrl, x2.getName());
            assertNotNull(x3);
            assertEquals(xxHrl, x3.getName());
            assertEquals(include, y1);
            assertEquals(include, y2);
            assertEquals(include, y3);
            assertNull(y4);
            assertEquals(include, y5);
            assertNull(y6);
            assertNull(z1);
            assertEquals(referencedInclude, z2);
            assertNull(z3);
            assertNull(z4);
            assertEquals(referencedInclude, z5);
            assertEquals(referencedInclude, z6);
        } finally {
            if (externalIncludeFile != null && externalIncludeFile.exists()) {
                externalIncludeFile.delete();
            }
            aProject.setIncludeDirs(includeDirs);
            final IProjectDescription description = workspaceProject
                    .getDescription();
            description.setReferencedProjects(referencedProjects);
            workspaceProject.setDescription(description, null);
        }
    }

    @Test
    public void findInclude_preferProjectFile() throws Exception {
        File externalIncludeFile = null;
        final IErlProject aProject = projects[0];
        final IProject workspaceProject = aProject.getWorkspaceProject();
        final IProject[] referencedProjects = workspaceProject
                .getReferencedProjects();
        final Collection<IPath> includeDirs = aProject.getIncludeDirs();
        // given
        // a project with an external include and a
        // referenced project with an include, both have same name
        try {
            final String xxHrl = "xx.hrl";
            externalIncludeFile = ErlideTestUtils.createTmpFile(xxHrl,
                    "-record(rec2, {field, another=def}.");
            final String externalIncludePath = externalIncludeFile
                    .getAbsolutePath();
            final IPath p = new Path(externalIncludePath).removeLastSegments(1);
            final List<IPath> newIncludeDirs = Lists.newArrayList(includeDirs);
            newIncludeDirs.add(p);
            aProject.setIncludeDirs(newIncludeDirs);
            final IErlProject project1 = projects[1];
            final IErlModule referencedInclude = ErlideTestUtils.createInclude(
                    project1, xxHrl, "-define(Z, referenced).\n");
            aProject.open(null);
            // when
            // looking for includes
            final String xx = "xx";
            final IErlModule x1 = aProject.findInclude(xx, null,
                    Scope.PROJECT_ONLY);
            final IErlModule x2 = aProject.findInclude(xx, null,
                    Scope.ALL_PROJECTS);
            final IErlModule x3 = aProject.findInclude(xx, null,
                    Scope.REFERENCED_PROJECTS);
            final IProjectDescription description = workspaceProject
                    .getDescription();
            description.setReferencedProjects(new IProject[] { project1
                    .getWorkspaceProject() });
            workspaceProject.setDescription(description, null);
            aProject.open(null);
            final IErlModule x4 = aProject.findInclude(xx, null,
                    Scope.PROJECT_ONLY);
            final IErlModule x5 = aProject.findInclude(xx, null,
                    Scope.ALL_PROJECTS);
            final IErlModule x6 = aProject.findInclude(xx, null,
                    Scope.REFERENCED_PROJECTS);
            // then
            // the non-external should be preferred
            assertNotNull(x1);
            assertEquals(xxHrl, x1.getName());
            assertNotSame(referencedInclude, x1);
            assertEquals(referencedInclude, x2);
            assertNotNull(x3);
            assertEquals(xxHrl, x3.getName());
            assertNotSame(referencedInclude, x3);
            assertNotNull(x4);
            assertNotSame(referencedInclude, x4);
            assertEquals(referencedInclude, x5);
            assertEquals(referencedInclude, x6);
        } finally {
            if (externalIncludeFile != null && externalIncludeFile.exists()) {
                externalIncludeFile.delete();
            }
            aProject.setIncludeDirs(includeDirs);
            final IProjectDescription description = workspaceProject
                    .getDescription();
            description.setReferencedProjects(referencedProjects);
            workspaceProject.setDescription(description, null);
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
