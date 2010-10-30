package org.erlide.ui;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IErlTypespec;
import org.erlide.core.erlang.util.ErlangFunction;
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

public class ErlModelUtilsTest {

	static IErlProject projects[] = null;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		// We set up projects here, it's quite costly
		final String name1 = "testproject1";
		final IErlProject erlProject1 = ErlideTestUtils.createErlProject(
				ErlideTestUtils.getTmpPath(name1), name1);
		final String name2 = "testproject2";
		final IErlProject erlProject2 = ErlideTestUtils.createErlProject(
				ErlideTestUtils.getTmpPath(name2), name2);
		projects = new IErlProject[] { erlProject1, erlProject2 };
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		for (final IErlProject project : projects) {
			ErlideTestUtils.deleteErlProject(project);
		}
	}

	@Before
	public void setUp() throws Exception {
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void getImportsAsListTest() throws Exception {
		// given
		// an erlang module with imports
		final IErlModule module = ErlideTestUtils.createErlModule(projects[0],
				"a.erl", "-module(a).\n-import(lists, [reverse/1, foldl/3].\n");
		module.open(null);
		// when
		// fetching imports as list of OtpErlangTuple
		final List<OtpErlangObject> imports = ErlModelUtils
				.getImportsAsList(module);
		// then
		// they should be returned
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
		// an erlang module with typedef
		final IErlModule moduleB = ErlideTestUtils
				.createErlModule(
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
				.createErlModule(projects[0], "d.erl",
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

	private OtpErlangTuple makeTuple2(final String functionName, final int arity) {
		return new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangAtom(functionName), new OtpErlangLong(arity) });
	}
}
