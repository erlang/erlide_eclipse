package org.erlide.core.model.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.erlide.core.model.erlang.IErlElement.Kind;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class IErlElementTest {

	private static IErlProject[] projects;

	private IErlModule module;
	private IErlProject project;

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
		project = projects[0];
		module = ErlideTestUtils.createModule(projects[0], "xx.erl",
				"-module(xx).\n-include(\"yy.hrl\").\n"
						+ "f(A) ->\n    lists:reverse(A).\n");
	}

	@After
	public void tearDown() throws Exception {
		ErlideTestUtils.deleteModules();
	}

	// boolean exists();
	@Test
	public void exists() throws Exception {
		final boolean exists = module.exists();
		module.getResource().delete(true, null);
		final boolean exists2 = module.exists();
		assertTrue(exists);
		assertFalse(exists2);
	}

	// IErlElement getAncestorOfKind(Kind kind);
	@Test
	public void getAncestorOfKind() throws Exception {
		module.open(null);
		final IErlElement element = module.getElementAtLine(3);
		final IErlElement ancestor = element.getAncestorOfKind(Kind.FUNCTION);
		final IErlElement ancestor2 = element.getAncestorOfKind(Kind.MODULE);
		final IErlElement ancestor3 = element.getAncestorOfKind(Kind.FOLDER);
		final IErlElement ancestor4 = element.getAncestorOfKind(Kind.PROJECT);
		final IErlElement ancestor5 = element.getAncestorOfKind(Kind.MODEL);
		final IErlElement ancestor6 = element.getAncestorOfKind(Kind.TYPESPEC);
		assertNotNull(ancestor);
		assertTrue(ancestor instanceof IErlFunction);
		assertEquals(Kind.FUNCTION, ancestor.getKind());
		assertEquals(element, ancestor);
		assertNotNull(ancestor2);
		assertEquals(Kind.MODULE, ancestor2.getKind());
		assertNotNull(ancestor3);
		assertEquals(Kind.FOLDER, ancestor3.getKind());
		assertNotNull(ancestor4);
		assertEquals(Kind.PROJECT, ancestor4.getKind());
		assertNotNull(ancestor5);
		assertEquals(Kind.MODEL, ancestor5.getKind());
		assertEquals(ancestor3, ancestor2.getAncestorOfKind(Kind.FOLDER));
		assertEquals(ancestor4, ancestor2.getAncestorOfKind(Kind.PROJECT));
		assertNull(ancestor6);
	}

	// IErlProject getProject();
	@Test
	public void getProject() throws Exception {
		assertEquals(project, module.getProject());
		assertEquals(project, project.getProject());
		module.open(null);
		final IErlElement element = module.getElementAtLine(3);
		assertEquals(project, element.getProject());
	}

	// IErlModule getModule();
	@Test
	public void getModule() throws Exception {
		assertEquals(module, module.getModule());
		assertNull(project.getModule());
		module.open(null);
		final IErlElement element = module.getElementAtLine(3);
		assertEquals(module, element.getModule());
	}

	// IResource getCorrespondingResource();
	@Test
	public void getCorrespondingResource() throws Exception {
		; // TODO
	}

	// String getName();
	// Kind getKind();
	// IErlModel getModel();
	// IParent getParent();
	// IResource getResource();
	// ISchedulingRule getSchedulingRule();
	// boolean isReadOnly();
	// boolean isStructureKnown() throws ErlModelException;
	// void resourceChanged(IResourceDelta delta);
	// void accept(IErlElementVisitor visitor, EnumSet<AcceptFlags> flags,
	// IErlElement.Kind leafKind) throws ErlModelException;
	// String getLabelString();
	// String getFilePath();
	// void clearCaches();
}
