package org.erlide.ui;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.List;

import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlImport;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IErlTypespec;
import org.erlide.core.erlang.util.ErlangFunction;
import org.erlide.core.text.ErlangToolkit;
import org.erlide.jinterface.backend.Backend;
import org.erlide.test.support.ErlideTestUtils;
import org.erlide.ui.util.ErlModelUtils;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideOpen;
import erlang.OpenResult;

public class ErlModelUtilsTest {

	static IErlProject projects[] = null;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		ErlideTestUtils.initProjects();
		// We set up projects here, it's quite costly
		final String name1 = "testproject1";
		final IErlProject erlProject1 = ErlideTestUtils.createProject(
				ErlideTestUtils.getTmpPath(name1), name1);
		final String name2 = "testproject2";
		final IErlProject erlProject2 = ErlideTestUtils.createProject(
				ErlideTestUtils.getTmpPath(name2), name2);
		projects = new IErlProject[] { erlProject1, erlProject2 };
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		ErlideTestUtils.deleteProjects();
	}

	@Before
	public void setUp() throws Exception {
		ErlideTestUtils.initModules();
	}

	@After
	public void tearDown() throws Exception {
		ErlideTestUtils.deleteModules();
	}

	@Test
	public void getImportsAsListTest() throws Exception {
		// given
		// an Erlang module with imports
		final IErlModule moduleA = ErlideTestUtils.createModule(projects[0],
				"a.erl", "-module(a).\n-import(lists, [reverse/1, foldl/3].\n");
		moduleA.open(null);
		// when
		// fetching imports as list of OtpErlangTuple
		final Collection<IErlElement> children = moduleA.getChildren();
		final Collection<IErlImport> imports2 = moduleA.getImports();
		final List<OtpErlangObject> imports = ErlModelUtils
				.getImportsAsList(moduleA);
		// then
		// they should be returned
		if (children.size() != 2) {
			System.out.println(moduleA);
			System.out.println(children);
		}
		assertEquals(2, children.size());
		assertEquals(1, imports2.size());
		assertEquals(1, imports.size());
		final OtpErlangAtom listAtom = new OtpErlangAtom("lists");
		assertEquals(
				new OtpErlangTuple(new OtpErlangObject[] {
						listAtom,
						new OtpErlangList(new OtpErlangObject[] {
								makeTuple2("reverse", 1),
								makeTuple2("foldl", 3) }) }), imports.get(0));
	}

	@Test
	public void findExternalTypeTest() throws Exception {
		// given
		// an Erlang module with typedef
		final IErlModule moduleB = ErlideTestUtils
				.createModule(
						projects[0],
						"b.erl",
						"-module(b).\n-type concat_thing() :: atom() | integer() | float() | string().\n");
		// final IErlModule moduleC =
		// ErlideTestUtils.createErlModule(projects[1],
		// "c.erl", "-module(c).\n-type cc() :: b:concat_thing().\n");
		moduleB.open(null);
		// moduleC.open(null);
		// when
		// looking for it
		// within project
		final IErlElement element1 = ErlModelUtils.findExternalType(moduleB,
				"b", "concat_thing", moduleB.getResource().getLocation()
						.toPortableString(), projects[0].getProject(), false);
		// in other project but path given
		final IErlElement element2 = ErlModelUtils.findExternalType(moduleB,
				"b", "concat_thing", moduleB.getResource().getLocation()
						.toPortableString(), projects[1].getProject(), false);
		// in other project no path given, search all projects true
		final IErlElement element3 = ErlModelUtils.findExternalType(moduleB,
				"b", "concat_thing", null, projects[1].getProject(), true);
		// in other project no path given, search all projects false, -> null
		final IErlElement element4 = ErlModelUtils.findExternalType(moduleB,
				"b", "concat_thing", null, projects[1].getProject(), false);
		// then
		// it should be returned if found
		assertTrue(element1 instanceof IErlTypespec);
		assertTrue(element2 instanceof IErlTypespec);
		assertTrue(element3 instanceof IErlTypespec);
		assertNull(element4);
	}

	@Test
	public void findExternalFunctionMODULETest() throws Exception {
		// given
		// a module with functions and functions
		final IErlModule moduleD = ErlideTestUtils
				.createModule(projects[0], "d.erl",
						"-module(d).\n-export([f/0]).\nf() ->\n    ok.\ng() ->\n    ?MODULE:f().\n");
		moduleD.open(null);
		// when
		// looking for it with ?MODULE
		final IErlElement element1 = ErlModelUtils.findExternalFunction(
				"?MODULE", new ErlangFunction("f", 0), null,
				projects[0].getProject(), false, moduleD);
		// then
		// it should be found
		assertTrue(element1 instanceof IErlFunction);
	}

	@Test
	public void findFunctionInExternalFilesTest() throws Exception {
		// given
		// a module with calls to the lists module
		final IErlProject project = projects[0];
		final IErlModule moduleE = ErlideTestUtils
				.createModule(
						project,
						"e.erl",
						"-module(e).\n-export([f/0]).\nf() ->\n    lists:reverse([1, 0]),\n    lists:reverse([1, 0], [2]).\n");
		moduleE.open(null);
		// when
		// looking for lists:reverse/2 and lists:reverse/1
		final Backend backend = ErlangCore.getBackendManager().getIdeBackend();
		final IErlModel model = ErlangCore.getModel();
		final OpenResult res = ErlideOpen.open(backend,
				ErlangToolkit.createScannerModuleName(moduleE), 49,
				ErlModelUtils.getImportsAsList(moduleE),
				model.getExternalModules(project), model.getPathVars());
		final IErlElement function = ErlModelUtils.findExternalFunction(
				res.getName(), res.getFunction(), res.getPath(),
				project.getProject(), false, moduleE);
		final OpenResult res2 = ErlideOpen.open(backend,
				ErlangToolkit.createScannerModuleName(moduleE), 81,
				ErlModelUtils.getImportsAsList(moduleE),
				model.getExternalModules(project), model.getPathVars());
		final IErlElement module = ErlModelUtils.findExternalFunction(
				res2.getName(), res2.getFunction(), res2.getPath(),
				project.getProject(), false, moduleE);
		// then
		// the function should be returned and the module, in External Files
		assertNotNull(function);
		assertTrue(function instanceof IErlFunction);
		assertNotNull(module);
		assertEquals(function.getParent(), module);
		// assertEquals(function.getParent().getParent().getResource(),
		// ResourceUtil.getExternalFilesProject()); FIXME
	}

	private OtpErlangTuple makeTuple2(final String functionName, final int arity) {
		return new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangAtom(functionName), new OtpErlangLong(arity) });
	}
}
