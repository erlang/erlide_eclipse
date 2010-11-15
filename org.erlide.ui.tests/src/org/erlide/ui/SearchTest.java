package org.erlide.ui;

import static org.junit.Assert.assertEquals;

import java.util.List;

import org.eclipse.core.resources.IResource;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.test.support.ErlideTestUtils;
import org.erlide.ui.internal.search.ErlSearchQuery;
import org.erlide.ui.internal.search.ErlangSearchElement;
import org.erlide.ui.internal.search.ErlangSearchResult;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

import erlang.ErlangSearchPattern;

public class SearchTest {

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
	public void findExternalCallsTest() throws Exception {
		// given
		// a module a with an exported function f
		// and a module b which calls a:f()
		final IErlModule moduleA = ErlideTestUtils.createErlModule(projects[0],
				"a.erl", "-module(a).\n-export([f/0]).\nf() ->\n    ok.\n");
		final IErlModule moduleB = ErlideTestUtils.createErlModule(projects[0],
				"b.erl", "-module(b).\n-export([f/0]).\nf() ->\n    a:f().\n");
		moduleA.open(null);
		moduleB.open(null);
		// when
		// searching for the call to a:f
		final ErlangSearchPattern ref = ErlangSearchPattern.getSearchPattern(
				ErlangSearchPattern.SEARCHFOR_FUNCTION, "a", "f", 0,
				ErlangSearchPattern.REFERENCES);
		final List<IResource> scope = Lists.newArrayList(moduleA.getResource(),
				moduleB.getResource());
		final ErlSearchQuery query = new ErlSearchQuery(ref, scope, "");
		query.run(null);
		final ErlangSearchResult searchResult = (ErlangSearchResult) query
				.getSearchResult();
		// then
		// it should be found in module b
		assertEquals(1, searchResult.getMatchCount());
		final List<ErlangSearchElement> result = searchResult.getResult();
	}

	private OtpErlangTuple makeTuple2(final String functionName, final int arity) {
		return new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangAtom(functionName), new OtpErlangLong(arity) });
	}
}
