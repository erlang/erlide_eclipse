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
import org.erlide.core.backend.runtimeinfo.RuntimeVersion;
import org.erlide.core.model.erlang.IErlProject.Scope;
import org.erlide.core.model.erlang.internal.ErlModelCache;
import org.erlide.core.model.erlang.internal.OldErlangProjectProperties;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

public class IErlProjectTests {

	private static final String XX_ERLIDEX = "xx.erlidex";
	private static IErlProject[] projects;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		ErlideTestUtils.initProjects();
		// We set up projects here, it's quite costly
		final String name1 = "testproject1";
		final IErlProject project1 = ErlideTestUtils.createProject(
				ErlideTestUtils.getTmpPath(name1), name1);
		final String name2 = "testproject2";
		final IErlProject project2 = ErlideTestUtils.createProject(
				ErlideTestUtils.getTmpPath(name2), name2);
		projects = new IErlProject[] { project1, project2 };
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		ErlideTestUtils.deleteProjects();
	}

	@Before
	public void setUp() throws Exception {
		ErlideTestUtils.initModulesAndIncludes();
	}

	@After
	public void tearDown() throws Exception {
		ErlideTestUtils.deleteModules();
	}

	// Collection<IErlModule> getModules() throws ErlModelException;
	@Test
	public void getModules() throws Exception {
		final IErlModule moduleAA = ErlideTestUtils.createModule(projects[0],
				"aa.erl", "-module(aa).\n");
		ErlideTestUtils.createInclude(projects[0], "bb.erl", "-module(bb).\n");
		ErlideTestUtils.createModule(projects[0], "cc.hrl",
				"-define(A, hej).\n");
		ErlideTestUtils.createInclude(projects[0], "dd.hrl",
				"-define(B, du).\n");
		final List<IErlModule> expected = Lists.newArrayList(moduleAA);
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
		final IErlModule moduleAA = ErlideTestUtils.createModule(projects[0],
				"aa.erl", "-module(aa).\n");
		ErlideTestUtils.createInclude(projects[0], "bb.erl", "-module(bb).\n");
		ErlideTestUtils.createModule(projects[0], "cc.hrl",
				"-define(A, hej).\n");
		final IErlModule includeD = ErlideTestUtils.createInclude(projects[0],
				"dd.hrl", "-define(B, du).\n");
		// FIXME should all of them be returned?
		final List<IErlModule> expected = Lists
				.newArrayList(moduleAA, includeD);
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
		final IErlProject project = projects[0];
		final String externalModulesString = project.getExternalModulesString();
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
			project.open(null);
			final Collection<IErlModule> otpModules = project
					.getExternalModules();
			project.setExternalModulesFile(externalsFile.getAbsolutePath());
			project.open(null);
			// when
			// fetching all external modules
			final Collection<IErlModule> externalModules = project
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
			project.setExternalModulesFile(externalModulesString);
		}
	}

	// Collection<IErlModule> getExternalIncludes() throws ErlModelException;
	// void setExternalIncludesFile(String absolutePath)
	// throws BackingStoreException;
	@Test
	public void getExternalIncludes() throws Exception {
		File externalFile = null;
		File externalsFile = null;
		final IErlProject project = projects[0];
		final String externalIncludesString = project
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
			project.open(null);
			final Collection<IErlModule> otpIncludes = project
					.getExternalIncludes();
			project.setExternalIncludesFile(externalsFile.getAbsolutePath());
			project.open(null);
			// when
			// fetching all external includes
			final Collection<IErlModule> externalIncludes = project
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
			project.setExternalIncludesFile(externalIncludesString);
		}
	}

	@Test
	public void getExternalIncludes_includeDirs() throws Exception {
		File externalFile = null;
		final IErlProject project = projects[0];
		final Collection<IPath> includeDirs = project.getIncludeDirs();
		try {
			// given
			// an erlang project and an external file not in any project, but on
			// the include-path
			final String externalFileName = "external.hrl";
			externalFile = ErlideTestUtils.createTmpFile(externalFileName,
					"-define(E, hej).\n");
			final String absolutePath = externalFile.getAbsolutePath();
			final List<IPath> newIncludeDirs = Lists.newArrayList(includeDirs);
			project.open(null);
			final Collection<IErlModule> otpIncludes = project
					.getExternalIncludes();
			final IPath absoluteDir = new Path(absolutePath)
					.removeLastSegments(1);
			newIncludeDirs.add(absoluteDir);
			project.setIncludeDirs(newIncludeDirs);
			project.open(null);
			// when
			// fetching all external includes
			final Collection<IErlModule> externalIncludes = project
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
			project.setIncludeDirs(includeDirs);
		}
	}

	// String getExternalModulesString();
	@Test
	public void getExternalModulesString() throws Exception {
		final IErlProject project = projects[0];
		final String externalIncludesString = project
				.getExternalIncludesString();
		try {
			final String s = "/hej";
			project.setExternalModulesFile(s);
			assertEquals(s, project.getExternalModulesString());
		} finally {
			project.setExternalModulesFile(externalIncludesString);
		}
	}

	// String getExternalIncludesString();
	@Test
	public void getExternalIncludesString() throws Exception {
		final IErlProject project = projects[0];
		final String externalIncludesString = project
				.getExternalIncludesString();
		try {
			final String s = "/tjo";
			project.setExternalIncludesFile(s);
			assertEquals(s, project.getExternalIncludesString());
		} finally {
			project.setExternalIncludesFile(externalIncludesString);
		}
	}

	// void setIncludeDirs(Collection<IPath> includeDirs)
	// throws BackingStoreException;
	@Test
	public void setIncludeDirs() throws Exception {
		File externalFile = null;
		final IErlProject project = projects[0];
		final Collection<IPath> includeDirs = project.getIncludeDirs();
		try {
			// given
			// an erlang project and an external file not in any project
			final String externalFileName = "external.hrl";
			externalFile = ErlideTestUtils.createTmpFile(externalFileName,
					"-define(E, hej).\n");
			final String absolutePath = externalFile.getAbsolutePath();
			final List<IPath> newIncludeDirs = Lists.newArrayList(includeDirs);
			project.open(null);
			final Collection<IErlModule> otpIncludes = project
					.getExternalIncludes();
			final IPath absoluteDir = new Path(absolutePath)
					.removeLastSegments(1);
			newIncludeDirs.add(absoluteDir);
			project.setIncludeDirs(newIncludeDirs);
			project.open(null);
			// when
			// fetching all external includes
			final Collection<IErlModule> externalIncludes = project
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
			project.setIncludeDirs(includeDirs);
		}
	}

	// void setSourceDirs(Collection<IPath> sourceDirs)
	// throws BackingStoreException;
	@Test
	public void setSourceDirs() throws Exception {
		final IErlProject project = projects[0];
		final Collection<IPath> sourceDirs = project.getSourceDirs();
		try {
			// given
			// an Erlang project and a module
			final IErlModule module = ErlideTestUtils.createModule(project,
					"aa.erl", "-module(aa).\n");
			final IPath srcxPath = new Path("srcx");
			final List<IPath> srcxDirs = Lists.newArrayList(srcxPath);
			project.open(null);
			// when
			// setting source dirs so the module is on source path
			final Collection<IErlModule> modules = project.getModules();
			project.setSourceDirs(srcxDirs);
			project.open(null);
			final Collection<IErlModule> srcxModules = project.getModules();
			project.setSourceDirs(sourceDirs);
			project.open(null);
			final Collection<IErlModule> modulesAgain = project.getModules();
			// then
			// the it should be returned, but not otherwise
			assertEquals(0, srcxModules.size());
			assertEquals(1, modules.size());
			assertEquals(module, modules.iterator().next());
			assertEquals(1, modulesAgain.size());
			assertEquals(module, modulesAgain.iterator().next());
		} finally {
			project.setSourceDirs(sourceDirs);
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
		final IErlProject project = projects[0];
		final RuntimeInfo info = project.getRuntimeInfo();
		final String expected = ResourcesPlugin.getWorkspace().getRoot()
				.getLocation().toString();
		assertNotNull(info);
		assertEquals(expected, info.getWorkingDir());
	}

	// RuntimeVersion getRuntimeVersion();
	@Test
	public void getRuntimeVersion() throws Exception {
		final IErlProject project = projects[0];
		final RuntimeVersion version = project.getRuntimeVersion();
		assertNotNull(version);
		final String major = version.asMajor().toString();
		assertTrue(major.startsWith("R"));
		final int majorVersion = Integer.valueOf(major.substring(1)).intValue();
		assertTrue(majorVersion >= 12);
	}

	// boolean hasSourceDir(IPath fullPath);
	@Test
	public void hasSourceDir() throws Exception {
		final IErlProject project = projects[0];
		final IPath dot = new Path(".");
		assertTrue(project.hasSourceDir(dot));
		final IPath projectPath = project.getWorkspaceProject().getFullPath();
		final IPath src = projectPath.append("src");
		assertTrue(project.hasSourceDir(src));
		final IPath srcx = projectPath.append("srcx");
		assertFalse(project.hasSourceDir(srcx));
		final IPath include = projectPath.append("include");
		assertFalse(project.hasSourceDir(include));
	}

	// TODO check more properties than source dirs property
	/**
	 * @see org.erlide.core.model.erlang.IErlProject#setAllProperties()
	 */
	@Test
	public void setAllProperties() throws Exception {
		final IErlProject project = projects[0];
		final Collection<IPath> sourceDirs = project.getSourceDirs();
		try {
			final OldErlangProjectProperties properties = new OldErlangProjectProperties(
					project.getWorkspaceProject());
			final IPath srcx = new Path("srcx");
			properties.setSourceDirs(Lists.newArrayList(srcx));
			project.setAllProperties(properties);
			final Collection<IPath> sourceDirs2 = project.getSourceDirs();
			assertEquals(1, sourceDirs2.size());
			assertEquals(srcx, sourceDirs2.iterator().next());
		} finally {
			project.setSourceDirs(sourceDirs);
		}
	}

	/**
	 * @see org.erlide.core.model.erlang.IErlProject#clearCaches()
	 */
	// TODO check more than source dir cache
	@Test
	public void clearCaches() throws Exception {
		final IErlProject project = projects[0];
		project.getSourceDirs();
		final ErlModelCache cache = ErlModelCache.getDefault();
		final Collection<IPath> sourceDirs = cache.getSourceDirs(project);
		project.clearCaches();
		final Collection<IPath> sourceDirs2 = cache.getSourceDirs(project);
		assertNotNull(sourceDirs);
		assertNull(sourceDirs2);
	}

	/**
	 * @see org.erlide.core.model.erlang.IErlProject#getReferencedProjects()
	 */
	@Test
	public void getReferencedProjects() throws Exception {
		final IProject project = projects[0].getWorkspaceProject();
		final IProjectDescription description = project.getDescription();
		final IProject[] refs = new IProject[] { projects[1]
				.getWorkspaceProject() };
		try {
			description.setReferencedProjects(refs);
			project.setDescription(description, null);
			final List<IErlProject> expected = Lists.newArrayList(projects[1]);
			assertEquals(expected, projects[0].getReferencedProjects());
		} finally {
			description.setReferencedProjects(new IProject[0]);
			project.setDescription(description, null);
		}
	}

	@Test(expected = ErlModelException.class)
	public void getProjectReferences_closedProject() throws Exception {
		final IErlProject erlProject = projects[0];
		final IProject project = erlProject.getWorkspaceProject();
		try {
			project.close(null);
			erlProject.getReferencedProjects();
		} finally {
			if (!project.isOpen()) {
				project.open(null);
			}
		}
	}

	// IErlModule getModule(String name) throws ErlModelException;
	@Test
	public void getModule() throws Exception {
		final IErlProject project = projects[0];
		final Collection<IPath> sourceDirs = project.getSourceDirs();
		try {
			// given
			// an Erlang project and a module
			final IErlModule module = ErlideTestUtils.createModule(project,
					"aa.erl", "-module(aa).\n");
			final IPath srcxPath = new Path("srcx");
			final List<IPath> srcxDirs = Lists.newArrayList(srcxPath);
			project.open(null);
			// when
			// setting source dirs so the module is on source path
			final IErlModule module2 = project.getModule("aa");
			final IErlModule nullModule = project.getModule("aa.hrl");
			final IErlModule nullModule2 = project.getModule("AA");
			final IErlModule nullModule3 = project.getModule("aA");
			final IErlModule nullModule4 = project.getModule("AA.erl");
			final IErlModule module4 = project.getModule("aa.erl");
			project.setSourceDirs(srcxDirs);
			project.open(null);
			final IErlModule srcxModule = project.getModule("aa");
			project.setSourceDirs(sourceDirs);
			project.open(null);
			final IErlModule module3 = project.getModule("aa");
			// then
			// the it should be returned, but not otherwise
			assertEquals(module, module2);
			assertNull(srcxModule);
			assertNull(nullModule);
			assertNull(nullModule2);
			assertNull(nullModule3);
			assertNull(nullModule4);
			assertEquals(module, module3);
			assertEquals(module, module4);
		} finally {
			project.setSourceDirs(sourceDirs);
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
		final IErlProject project = projects[0];
		final IProject workspaceProject = project.getWorkspaceProject();
		final IProject[] referencedProjects = workspaceProject
				.getReferencedProjects();
		final String externalModulesString = project.getExternalModulesString();
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
			project.setExternalModulesFile(externalsFile.getAbsolutePath());
			final IErlModule module = ErlideTestUtils.createModule(project,
					"yy.erl", "-module(yy).\n");
			final IErlProject project1 = projects[1];
			final IErlModule referencedModule = ErlideTestUtils.createModule(
					project1, "zz.erl", "-module(zz).\n");
			project.open(null);
			// when
			// looking for modules
			final String xx = "xx";
			final IErlModule x1 = project.findModule(xx, null,
					Scope.PROJECT_ONLY);
			final IErlModule x2 = project.findModule(xx, null,
					Scope.ALL_PROJECTS);
			final IErlModule x3 = project.findModule(xx, null,
					Scope.REFERENCED_PROJECTS);
			final String yy = "yy";
			final IErlModule y1 = project.findModule(yy, null,
					Scope.PROJECT_ONLY);
			final IErlModule y2 = project.findModule(yy, null,
					Scope.ALL_PROJECTS);
			final IErlModule y3 = project.findModule(yy, null,
					Scope.REFERENCED_PROJECTS);
			final IErlModule y4 = project1.findModule(yy, null,
					Scope.PROJECT_ONLY);
			final IErlModule y5 = project1.findModule(yy, null,
					Scope.ALL_PROJECTS);
			final IErlModule y6 = project1.findModule(yy, null,
					Scope.REFERENCED_PROJECTS);
			final String zz = "zz";
			final IErlModule z1 = project.findModule(zz, null,
					Scope.PROJECT_ONLY);
			final IErlModule z2 = project.findModule(zz, null,
					Scope.ALL_PROJECTS);
			final IErlModule z3 = project.findModule(zz, null,
					Scope.REFERENCED_PROJECTS);
			final IProjectDescription description = workspaceProject
					.getDescription();
			description.setReferencedProjects(new IProject[] { project1
					.getWorkspaceProject() });
			workspaceProject.setDescription(description, null);
			project.open(null);
			final IErlModule z4 = project.findModule(zz, null,
					Scope.PROJECT_ONLY);
			final IErlModule z5 = project.findModule(zz, null,
					Scope.ALL_PROJECTS);
			final IErlModule z6 = project.findModule(zz, null,
					Scope.REFERENCED_PROJECTS);
			// then
			// scope should be respected
			assertNotNull(x1);
			assertEquals(xxErl, x1.getName());
			assertNotNull(x2);
			assertEquals(xxErl, x2.getName());
			assertNotNull(x3);
			assertEquals(xxErl, x3.getName());
			assertEquals(module, y1);
			assertEquals(module, y2);
			assertEquals(module, y3);
			assertNull(y4);
			assertEquals(module, y5);
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
			project.setExternalModulesFile(externalModulesString);
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
		final IErlProject project = projects[0];
		final IProject workspaceProject = project.getWorkspaceProject();
		final IProject[] referencedProjects = workspaceProject
				.getReferencedProjects();
		final String externalModulesString = project.getExternalModulesString();
		// given
		// a project with an external include and a
		// referenced project with an include, both have same name
		try {
			final String xxErl = "xx.erl";
			final String xxContents = "-module(xx).\n";
			externalModuleFile = ErlideTestUtils.createTmpFile(xxErl,
					xxContents);
			final String externalModulePath = externalModuleFile
					.getAbsolutePath();
			externalsFile = ErlideTestUtils.createTmpFile(XX_ERLIDEX,
					externalModulePath);
			project.setExternalModulesFile(externalsFile.getAbsolutePath());
			final IErlProject project1 = projects[1];
			final IErlModule referencedModule = ErlideTestUtils.createModule(
					project1, xxErl, xxContents);
			project.open(null);
			// when
			// looking for module
			final String xx = "xx";
			final IErlModule xx1 = project.findModule(xx, null,
					Scope.PROJECT_ONLY);
			final IErlModule xx2 = project.findModule(xx, null,
					Scope.ALL_PROJECTS);
			final IErlModule xx3 = project.findModule(xx, null,
					Scope.REFERENCED_PROJECTS);
			final IProjectDescription description = workspaceProject
					.getDescription();
			description.setReferencedProjects(new IProject[] { project1
					.getWorkspaceProject() });
			workspaceProject.setDescription(description, null);
			project.open(null);
			final IErlModule xx4 = project.findModule(xx, null,
					Scope.PROJECT_ONLY);
			final IErlModule xx5 = project.findModule(xx, null,
					Scope.ALL_PROJECTS);
			final IErlModule xx6 = project.findModule(xx, null,
					Scope.REFERENCED_PROJECTS);
			// then
			// the non-external should be preferred
			assertNotNull(xx1);
			assertEquals(xxErl, xx1.getName());
			assertNotSame(referencedModule, xx1);
			assertEquals(referencedModule, xx2);
			assertNotNull(xx3);
			assertEquals(xxErl, xx3.getName());
			assertNotSame(referencedModule, xx3);
			assertNotNull(xx4);
			assertNotSame(referencedModule, xx4);
			assertEquals(referencedModule, xx5);
			assertEquals(referencedModule, xx6);
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
			project.setExternalModulesFile(externalModulesString);
		}
	}

	// IErlModule findInclude(String includeName, String includePath, Scope
	// scope)
	// throws ErlModelException;
	@Test
	public void findInclude() throws Exception {
		File externalIncludeFile = null;
		final IErlProject project = projects[0];
		final IProject workspaceProject = project.getWorkspaceProject();
		final IProject[] referencedProjects = workspaceProject
				.getReferencedProjects();
		final Collection<IPath> includeDirs = project.getIncludeDirs();
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
			project.setIncludeDirs(newIncludeDirs);
			final IErlModule include = ErlideTestUtils.createInclude(project,
					"yy.hrl", "-define(Y, include).\n");
			final IErlProject project1 = projects[1];
			final IErlModule referencedInclude = ErlideTestUtils.createInclude(
					project1, "zz.hrl", "-define(Z, referenced).\n");
			project.open(null);
			// when
			// looking for includes
			final String xx = "xx";
			final IErlModule x1 = project.findInclude(xx, null,
					Scope.PROJECT_ONLY);
			final IErlModule x2 = project.findInclude(xx, null,
					Scope.ALL_PROJECTS);
			final IErlModule x3 = project.findInclude(xx, null,
					Scope.REFERENCED_PROJECTS);
			final String yy = "yy";
			final IErlModule y1 = project.findInclude(yy, null,
					Scope.PROJECT_ONLY);
			final IErlModule y2 = project.findInclude(yy, null,
					Scope.ALL_PROJECTS);
			final IErlModule y3 = project.findInclude(yy, null,
					Scope.REFERENCED_PROJECTS);
			final IErlModule y4 = project1.findInclude(yy, null,
					Scope.PROJECT_ONLY);
			final IErlModule y5 = project1.findInclude(yy, null,
					Scope.ALL_PROJECTS);
			final IErlModule y6 = project1.findInclude(yy, null,
					Scope.REFERENCED_PROJECTS);
			final String zz = "zz";
			final IErlModule z1 = project.findInclude(zz, null,
					Scope.PROJECT_ONLY);
			final IErlModule z2 = project.findInclude(zz, null,
					Scope.ALL_PROJECTS);
			final IErlModule z3 = project.findInclude(zz, null,
					Scope.REFERENCED_PROJECTS);
			final IProjectDescription description = workspaceProject
					.getDescription();
			description.setReferencedProjects(new IProject[] { project1
					.getWorkspaceProject() });
			workspaceProject.setDescription(description, null);
			project.open(null);
			final IErlModule z4 = project.findInclude(zz, null,
					Scope.PROJECT_ONLY);
			final IErlModule z5 = project.findInclude(zz, null,
					Scope.ALL_PROJECTS);
			final IErlModule z6 = project.findInclude(zz, null,
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
			project.setIncludeDirs(includeDirs);
			final IProjectDescription description = workspaceProject
					.getDescription();
			description.setReferencedProjects(referencedProjects);
			workspaceProject.setDescription(description, null);
		}
	}

	@Test
	public void findInclude_preferProjectFile() throws Exception {
		File externalIncludeFile = null;
		final IErlProject project = projects[0];
		final IProject workspaceProject = project.getWorkspaceProject();
		final IProject[] referencedProjects = workspaceProject
				.getReferencedProjects();
		final Collection<IPath> includeDirs = project.getIncludeDirs();
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
			project.setIncludeDirs(newIncludeDirs);
			final IErlProject project1 = projects[1];
			final IErlModule referencedInclude = ErlideTestUtils.createInclude(
					project1, xxHrl, "-define(Z, referenced).\n");
			project.open(null);
			// when
			// looking for includes
			final String xx = "xx";
			final IErlModule x1 = project.findInclude(xx, null,
					Scope.PROJECT_ONLY);
			final IErlModule x2 = project.findInclude(xx, null,
					Scope.ALL_PROJECTS);
			final IErlModule x3 = project.findInclude(xx, null,
					Scope.REFERENCED_PROJECTS);
			final IProjectDescription description = workspaceProject
					.getDescription();
			description.setReferencedProjects(new IProject[] { project1
					.getWorkspaceProject() });
			workspaceProject.setDescription(description, null);
			project.open(null);
			final IErlModule x4 = project.findInclude(xx, null,
					Scope.PROJECT_ONLY);
			final IErlModule x5 = project.findInclude(xx, null,
					Scope.ALL_PROJECTS);
			final IErlModule x6 = project.findInclude(xx, null,
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
			project.setIncludeDirs(includeDirs);
			final IProjectDescription description = workspaceProject
					.getDescription();
			description.setReferencedProjects(referencedProjects);
			workspaceProject.setDescription(description, null);
		}
	}

	// IProject getWorkspaceProject();
	@Test
	public void getWorkspaceProject() throws Exception {
		final IErlProject project = projects[0];
		final IProject workspaceProject = project.getWorkspaceProject();
		assertNotNull(workspaceProject);
		assertEquals(project.getName(), workspaceProject.getName());
	}
}
