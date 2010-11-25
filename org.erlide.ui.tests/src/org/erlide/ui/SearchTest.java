package org.erlide.ui;

import static org.junit.Assert.assertEquals;

import java.util.List;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.OperationCanceledException;
import org.erlide.core.erlang.ErlModelException;
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

import com.google.common.collect.Lists;

import erlang.ErlangSearchPattern;
import erlang.ErlangSearchPattern.LimitTo;
import erlang.ErlangSearchPattern.SearchFor;

public class SearchTest {

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
	public void findExternalCallsReferencesTest() throws Exception {
		findExternalCallsTestAux(LimitTo.REFERENCES, 1);
	}

	@Test
	public void findExternalCallsDefinitionsTest() throws Exception {
		findExternalCallsTestAux(LimitTo.DEFINITIONS, 2);
	}

	@Test
	public void findExternalCallsAllOccurencesTest() throws Exception {
		findExternalCallsTestAux(LimitTo.ALL_OCCURRENCES, 3);
	}

	private void findExternalCallsTestAux(final LimitTo limitTo,
			final int nFoundExpected) throws CoreException, ErlModelException,
			OperationCanceledException {
		// given
		// a module a with an exported function f
		// and a module b which calls a:f()
		final IErlModule moduleA = ErlideTestUtils.createModule(projects[0],
				"a.erl", "-module(a).\n-export([f/0]).\nf() ->\n    ok.\n");
		final IErlModule moduleB = ErlideTestUtils.createModule(projects[0],
				"b.erl", "-module(b).\n-export([f/0]).\nf() ->\n    a:f().\n");
		moduleA.open(null);
		moduleB.open(null);
		// when
		// searching for the call to a:f
		final ErlangSearchPattern ref = ErlangSearchPattern.getSearchPattern(
				SearchFor.FUNCTION, "a", "f", 0, limitTo);
		final List<IResource> scope = Lists.newArrayList(moduleA.getResource(),
				moduleB.getResource());
		final ErlSearchQuery query = new ErlSearchQuery(ref, scope, null, "");
		query.run(null);
		// then
		// it should be found in module b
		final ErlangSearchResult searchResult = (ErlangSearchResult) query
				.getSearchResult();
		assertEquals(nFoundExpected, searchResult.getMatchCount());
		final List<ErlangSearchElement> result = searchResult.getResult();
	}
}
