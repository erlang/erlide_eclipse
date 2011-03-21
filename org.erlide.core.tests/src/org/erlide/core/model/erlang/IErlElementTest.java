package org.erlide.core.model.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.erlide.core.ErlangCore;
import org.erlide.core.model.erlang.IErlElement.Kind;
import org.erlide.core.model.erlang.IErlProject.Scope;
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
		assertEquals(Kind.MODULE, ancestor2.getKind());
		assertEquals(Kind.FOLDER, ancestor3.getKind());
		assertEquals(Kind.PROJECT, ancestor4.getKind());
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
		project.open(null);
		final IProject workspaceProject = project.getWorkspaceProject();
		final IFolder srcFolder = workspaceProject.getFolder("src");
		final IFile file = srcFolder.getFile("xx.erl");
		final IErlModule otpFile = project.findModule("file.erl", null,
				Scope.PROJECT_ONLY);
		module.open(null);
		final IErlElement element = module.getElementAtLine(3);
		assertEquals(file, module.getCorrespondingResource());
		assertNull(otpFile.getCorrespondingResource());
		assertNull(element.getCorrespondingResource());
	}

	// String getName();
	@Test
	public void getName() throws Exception {
		module.open(null);
		final IErlElement element = module.getElementAtLine(3);
		final IErlElement element2 = module.getElementAtLine(0);
		assertEquals("xx.erl", module.getName());
		assertEquals("testproject1", project.getName());
		assertEquals("f", element.getName());
		assertEquals("module", element2.getName());
	}

	// Kind getKind();
	@Test
	public void getKind() throws Exception {
		module.open(null);
		final IErlElement element = module.getElementAtLine(3);
		final IErlElement element2 = module.getElementAtLine(0);
		assertEquals(Kind.MODULE, module.getKind());
		assertEquals(Kind.PROJECT, project.getKind());
		assertEquals(Kind.FUNCTION, element.getKind());
		assertEquals(Kind.ATTRIBUTE, element2.getKind());
		assertEquals(Kind.MODEL, element2.getModel().getKind());
	}

	// IErlModel getModel();
	@Test
	public void getModel() throws Exception {
		module.open(null);
		final IErlElement element = module.getElementAtLine(3);
		final IErlModel model = ErlangCore.getModel();
		assertEquals(model, project.getModel());
		assertEquals(model, module.getModel());
		assertEquals(model, element.getModel());
	}

	// IParent getParent();
	@Test
	public void getParent() throws Exception {
		project.open(null);
		final IErlElement srcFolder = project.getChildNamed("src");
		module.open(null);
		final IErlElement element = module.getElementAtLine(3);
		assertEquals(project, srcFolder.getParent());
		assertEquals(srcFolder, module.getParent());
		assertEquals(module, element.getParent());
	}

	// IResource getResource();
	@Test
	public void getResource() throws Exception {
		project.open(null);
		final IProject workspaceProject = project.getWorkspaceProject();
		final IFolder srcFolder = workspaceProject.getFolder("src");
		final IFile file = srcFolder.getFile("xx.erl");
		final IErlModule otpFile = project.findModule("file.erl", null,
				Scope.PROJECT_ONLY);
		module.open(null);
		final IErlElement element = module.getElementAtLine(3);
		assertEquals(file, module.getResource());
		assertNull(otpFile.getResource());
		assertEquals(module.getResource(), element.getResource());
	}

	// ISchedulingRule getSchedulingRule();
	@Test
	public void getSchedulingRule() throws Exception {
		project.open(null);
		final IErlModule otpFile = project.findModule("file.erl", null,
				Scope.PROJECT_ONLY);
		module.open(null);
		final IErlElement element = module.getElementAtLine(3);
		// TODO more testing here
		final ISchedulingRule schedulingRule = module.getSchedulingRule();
		assertNotNull(schedulingRule);
		assertNotNull(otpFile);
		assertEquals(schedulingRule, element.getSchedulingRule());
		assertNotNull(otpFile.getSchedulingRule());
		assertNull(otpFile.getSchedulingRule());
	}

	// boolean isReadOnly();
	// Empty method

	// boolean isStructureKnown() throws ErlModelException;
	@Test
	public void isStructureKnown() throws Exception {
		project.setSourceDirs(project.getSourceDirs());
		// this sets structureKnown to false

		final boolean structureKnown = project.isStructureKnown();
		// FIXME shouldn't open below be enough?
		project.open(null);
		final boolean structureKnown2 = project.isStructureKnown();
		final boolean structureKnown3 = module.isStructureKnown();
		module.open(null);
		final boolean structureKnown4 = module.isStructureKnown();
		final IErlModule otpFile = project.findModule("file.erl", null,
				Scope.PROJECT_ONLY);
		final IErlExternal external = (IErlExternal) otpFile.getParent();
		final boolean structureKnown5 = external.isStructureKnown();
		final IErlModule module2 = ErlideTestUtils
				.createModule(project, "yy.erl",
						"-module(yy).\n% comment\n% same\nf(x) -> y.\n% last");
		final boolean structureKnown6 = module2.isStructureKnown();
		module2.open(null);
		final boolean structureKnown7 = module2.isStructureKnown();
		assertFalse(structureKnown);
		assertTrue(structureKnown2);
		assertFalse(structureKnown3);
		assertTrue(structureKnown4);
		assertTrue(structureKnown5);
		assertFalse(structureKnown6);
		assertTrue(structureKnown7);
	}

	// void resourceChanged(IResourceDelta delta);
	// void accept(IErlElementVisitor visitor, EnumSet<AcceptFlags> flags,
	// IErlElement.Kind leafKind) throws ErlModelException;
	// String getLabelString();
	// String getFilePath();
	// void clearCaches();
}
