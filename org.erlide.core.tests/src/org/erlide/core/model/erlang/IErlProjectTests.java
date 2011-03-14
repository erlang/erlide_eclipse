package org.erlide.core.model.erlang;

import static org.junit.Assert.assertEquals;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.google.common.collect.Lists;

public class IErlProjectTests {

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
		final IErlModule moduleA = ErlideTestUtils.createModule(projects[0],
				"a.erl", "-module(a).\n");
		// final IErlModule moduleB =
		ErlideTestUtils.createInclude(projects[0], "b.erl", "-module(b).\n");
		// final IErlModule includeC =
		ErlideTestUtils
				.createModule(projects[0], "c.hrl", "-define(A, hej).\n");
		// final IErlModule includeD =
		ErlideTestUtils
				.createInclude(projects[0], "d.hrl", "-define(B, du).\n");
		final List<IErlModule> expected = Lists.newArrayList(moduleA);
		final Collection<IErlModule> modules = projects[0].getModules();
		assertEquals(expected, modules);
	}

	// FIXME lägg till test som ger ErlModelException, t.ex. genom att döpa om
	// src-mappen före getModules()

	// TODO samma nedan, fast include-mappen

	// Collection<IErlModule> getIncludes() throws ErlModelException;
	@Test
	public void getIncludes() throws Exception {
		// final IErlModule moduleA =
		ErlideTestUtils.createModule(projects[0], "a.erl", "-module(a).\n");
		// final IErlModule moduleB =
		ErlideTestUtils.createInclude(projects[0], "b.erl", "-module(b).\n");
		// final IErlModule includeC =
		ErlideTestUtils
				.createModule(projects[0], "c.hrl", "-define(A, hej).\n");
		final IErlModule includeD = ErlideTestUtils.createInclude(projects[0],
				"d.hrl", "-define(B, du).\n");
		final List<IErlModule> expected = Lists.newArrayList(includeD);
		final Collection<IErlModule> includes = projects[0].getIncludes();
		assertEquals(expected, includes);
	}

	// Collection<IErlModule> getModulesAndIncludes() throws ErlModelException;
	@Test
	public void getModulesAndIncludes() throws Exception {
		final IErlModule moduleA = ErlideTestUtils.createModule(projects[0],
				"a.erl", "-module(a).\n");
		final IErlModule moduleB = ErlideTestUtils.createInclude(projects[0],
				"b.erl", "-module(b).\n");
		final IErlModule includeC = ErlideTestUtils.createModule(projects[0],
				"c.hrl", "-define(A, hej).\n");
		final IErlModule includeD = ErlideTestUtils.createInclude(projects[0],
				"d.hrl", "-define(B, du).\n");
		// FIXME should all of them be returned?
		final List<IErlModule> expected = Lists.newArrayList(moduleA, includeD);
		final Collection<IErlModule> includes = projects[0]
				.getModulesAndIncludes();
		assertEquals(expected, includes);
	}

	// Collection<IErlModule> getExternalModules() throws ErlModelException;
	// Collection<IErlModule> getExternalIncludes() throws ErlModelException;
	// String getExternalModulesString();
	// String getExternalIncludesString();
	// void setIncludeDirs(Collection<IPath> includeDirs)
	// throws BackingStoreException;
	// void setSourceDirs(Collection<IPath> sourceDirs)
	// throws BackingStoreException;
	// void setExternalModulesFile(String absolutePath)
	// throws BackingStoreException;
	// void setExternalIncludesFile(String absolutePath)
	// throws BackingStoreException;

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
	// RuntimeVersion getRuntimeVersion();
	// boolean hasSourceDir(IPath fullPath);
	// void setAllProperties(IOldErlangProjectProperties bprefs)
	// throws BackingStoreException;
	// void clearCaches();

	// Collection<IErlProject> getProjectReferences() throws ErlModelException;
	@Test
	public void getProjectReferences() throws Exception {
		final IProject project = projects[0].getWorkspaceProject();
		final IProjectDescription description = project.getDescription();
		final IProject[] refs = new IProject[] { projects[1]
				.getWorkspaceProject() };
		try {
			description.setReferencedProjects(refs);
			project.setDescription(description, null);
			final List<IErlProject> expected = Lists.newArrayList(projects[1]);
			assertEquals(expected, projects[0].getProjectReferences());
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
			erlProject.getProjectReferences();
		} finally {
			if (!project.isOpen()) {
				project.open(null);
			}
		}
	}

	// IErlModule getModule(String name) throws ErlModelException;
	// enum Scope {
	// PROJECT_ONLY, REFERENCED_PROJECTS, ALL_PROJECTS
	// }
	// IErlModule findModule(String moduleName, String modulePath, Scope scope)
	// throws ErlModelException;
	// IErlModule findInclude(String includeName, String includePath, Scope
	// scope)
	// throws ErlModelException;
	// IProject getWorkspaceProject();

}
