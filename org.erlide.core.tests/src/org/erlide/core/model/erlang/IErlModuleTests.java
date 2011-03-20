package org.erlide.core.model.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.Iterator;

import org.eclipse.core.resources.IResource;
import org.erlide.core.model.erlang.IErlElement.Kind;
import org.erlide.core.model.erlang.util.ErlangFunction;
import org.erlide.core.model.erlang.util.ErlangIncludeFile;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class IErlModuleTests {

	private static final String XX_ERLIDEX = "xx.erlidex";
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
		module = ErlideTestUtils
				.createModule(projects[0], "xx.erl",
						"-module(xx).\n-include(\"yy.hrl\").\nf(A) ->\n    lists:reverse(A).\n");
	}

	@After
	public void tearDown() throws Exception {
		ErlideTestUtils.deleteModules();
	}

	// IErlElement getElementAt(int position) throws ErlModelException;
	@Test
	public void getElementAt() throws Exception {
		module.open(null);
		final IErlElement element = module.getElementAt(0);
		final IErlElement element1 = module.getElementAt(14);
		final IErlElement element2 = module.getElementAt(1000);
		final IErlElement element3 = module.getElementAt(50);
		assertNotNull(element);
		assertNotNull(element1);
		assertTrue(element instanceof IErlAttribute);
		assertTrue(element1 instanceof IErlAttribute);
		assertEquals("include: \"yy.hrl\"", element1.toString());
		assertNull(element2);
		assertNotNull(element3);
		assertTrue(element3 instanceof IErlFunction);
	}

	// IErlElement getElementAtLine(int lineNumber);
	@Test
	public void getElementAtLine() throws Exception {
		module.open(null);
		final IErlElement element = module.getElementAtLine(0);
		final IErlElement element1 = module.getElementAtLine(1);
		final IErlElement element2 = module.getElementAtLine(4);
		final IErlElement element3 = module.getElementAtLine(3);
		assertNotNull(element);
		assertNotNull(element1);
		assertTrue(element instanceof IErlAttribute);
		assertTrue(element1 instanceof IErlAttribute);
		assertEquals("include: \"yy.hrl\"", element1.toString());
		assertNull(element2);
		assertNotNull(element3);
		assertTrue(element3 instanceof IErlFunction);
	}

	// ModuleKind getModuleKind();
	@Test
	public void getModuleKind() throws Exception {
		// TODO more tests
		assertEquals(ModuleKind.ERL, module.getModuleKind());
	}

	// Collection<IErlComment> getComments();
	@Test
	public void getComments() throws Exception {
		final IErlModule commentModule = ErlideTestUtils
				.createModule(project, "yy.erl",
						"-module(yy).\n% comment\n% same\nf(x) -> y.\n% last");
		commentModule.open(null);
		final Collection<IErlComment> comments = commentModule.getComments();
		final String c1 = "% comment\n% same";
		final String c2 = "% last";
		assertEquals(2, comments.size());
		final Iterator<IErlComment> iterator = comments.iterator();
		assertEquals(c1, iterator.next().getName());
		assertEquals(c2, iterator.next().getName());
	}

	// long getTimestamp();
	@Test
	public void getTimestamp() throws Exception {
		module.open(null);
		final long timestamp = module.getTimestamp();
		final IErlModule module2 = ErlideTestUtils.createModule(project,
				"yy.erl", "-module(yy).\n");
		module2.open(null);
		assertNotSame(IResource.NULL_STAMP, timestamp);
		assertTrue(timestamp <= module2.getTimestamp());
	}

	// IErlImport findImport(ErlangFunction function);
	@Test
	public void findImport() throws Exception {
		final IErlModule importModule = ErlideTestUtils
				.createModule(project, "yy.erl",
						"-module(yy).\n-import(lists, [reverse/1]).\nf(L) -> reverse(L).\n");
		module.open(null);
		importModule.open(null);
		final ErlangFunction reverse_1 = new ErlangFunction("reverse", 1);
		final IErlImport import1 = module.findImport(reverse_1);
		final IErlImport import2 = importModule.findImport(reverse_1);
		final ErlangFunction reverse_2 = new ErlangFunction("reverse", 2);
		final IErlImport import3 = importModule.findImport(reverse_2);
		assertNull(import1);
		assertNotNull(import2);
		assertNull(import3);
		assertEquals(import2.getFunctions().iterator().next(), reverse_1);
	}

	// Collection<IErlImport> getImports();
	@Test
	public void getImports() throws Exception {
		final IErlModule importModule = ErlideTestUtils
				.createModule(project, "yy.erl",
						"-module(yy).\n-import(lists, [reverse/1]).\nf(L) -> reverse(L).\n");
		module.open(null);
		importModule.open(null);
		final Collection<IErlImport> imports = module.getImports();
		final Collection<IErlImport> imports2 = importModule.getImports();
		assertEquals(0, imports.size());
		assertEquals(1, imports2.size());
	}

	// IErlPreprocessorDef findPreprocessorDef(String definedName, Kind kind);
	@Test
	public void findPreprocessorDef() throws Exception {
		final IErlModule preprocessorDefModule = ErlideTestUtils
				.createModule(
						project,
						"yy.erl",
						"-module(yy).\n-define(A, hej).\n-define(B(x), x).\n-record(?MODULE, {a, b}).\nf(L) -> reverse(L).\n");
		preprocessorDefModule.open(null);
		final IErlPreprocessorDef def1 = preprocessorDefModule
				.findPreprocessorDef("A", Kind.MACRO_DEF);
		final IErlPreprocessorDef def2 = preprocessorDefModule
				.findPreprocessorDef("A", Kind.RECORD_DEF);
		final IErlPreprocessorDef def3 = preprocessorDefModule
				.findPreprocessorDef("B", Kind.MACRO_DEF);
		final IErlPreprocessorDef def4 = preprocessorDefModule
				.findPreprocessorDef("?MODULE", Kind.RECORD_DEF);
		assertNotNull(def1);
		assertNull(def2);
		assertNotNull(def3);
		assertEquals("B", def3.getDefinedName());
		assertNotNull(def4);
	}

	// public Collection<IErlPreprocessorDef> getPreprocessorDefs(final Kind
	// kind);
	@Test
	public void getPreprocessorDefs() throws Exception {
		final IErlModule preprocessorDefModule = ErlideTestUtils
				.createModule(
						project,
						"yy.erl",
						"-module(yy).\n-define(A, hej).\n-define(B(x), x).\n-record(?MODULE, {a, b}).\nf(L) -> reverse(L).\n");
		preprocessorDefModule.open(null);
		final Collection<IErlPreprocessorDef> records = preprocessorDefModule
				.getPreprocessorDefs(Kind.RECORD_DEF);
		final Collection<IErlPreprocessorDef> macros = preprocessorDefModule
				.getPreprocessorDefs(Kind.MACRO_DEF);
		assertEquals(1, records.size());
		assertEquals(2, macros.size());
		final Iterator<IErlPreprocessorDef> iterator = macros.iterator();
		assertEquals("A", iterator.next().getDefinedName());
		assertEquals("B", iterator.next().getDefinedName());
	}

	// Collection<ErlangIncludeFile> getIncludedFiles() throws
	// ErlModelException;
	@Test
	public void getIncludedFiles() throws Exception {
		// ErlideTestUtils.createInclude(project, "yy.hrl",
		// "-define(A, hej).\n");
		final IErlModule includeLibModule = ErlideTestUtils
				.createModule(project, "zz.erl",
						"-module(zz).\n-include_lib(\"kernel/include/file.hrl\").\nf(_) -> ok");
		module.open(null);
		includeLibModule.open(null);
		final Collection<ErlangIncludeFile> includeFiles = module
				.getIncludeFiles();
		final Collection<ErlangIncludeFile> includeFiles2 = includeLibModule
				.getIncludeFiles();
		assertEquals(1, includeFiles.size());
		assertEquals("yy.hrl", includeFiles.iterator().next()
				.getFilenameLastPart());
		assertEquals(1, includeFiles2.size());
		assertEquals("file.hrl", includeFiles2.iterator().next()
				.getFilenameLastPart());
	}

	// void getScanner();
	// void disposeScanner();
	// void initialReconcile();
	// void reconcileText(int offset, int removeLength, String newText,
	// IProgressMonitor mon);
	// void postReconcile(IProgressMonitor mon);
	// void finalReconcile();
	// Set<IErlModule> getDirectDependents() throws ErlModelException;
	// Set<IErlModule> getAllDependents() throws ErlModelException;
	// void resetAndCacheScannerAndParser(String newText) throws
	// ErlModelException;
	// String getModuleName();
	// IErlFunction findFunction(ErlangFunction erlangFunction);
	// IErlTypespec findTypespec(String typeName);
	// ErlToken getScannerTokenAt(int offset);
	// void setResource(IFile file);
	// String getInitialText();
	// void addComment(IErlComment c);
	// List<IErlModule> findAllIncludedFiles() throws CoreException;
	// boolean isOnSourcePath();
	// boolean isOnIncludePath();
	// IErlModule findInclude(String includeName, String includePath, Scope
	// scope)
	// throws ErlModelException;

}
