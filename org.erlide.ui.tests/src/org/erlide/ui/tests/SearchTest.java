package org.erlide.ui.tests;

import static org.junit.Assert.*;

import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.services.search.ErlSearchScope;
import org.erlide.core.services.search.ErlangSearchPattern;
import org.erlide.core.services.search.ErlangSearchPattern.LimitTo;
import org.erlide.core.services.search.ErlangSearchPattern.SearchFor;
import org.erlide.test.support.ErlideTestUtils;
import org.erlide.ui.internal.search.ErlSearchQuery;
import org.erlide.ui.internal.search.ErlangSearchElement;
import org.erlide.ui.internal.search.ErlangSearchResult;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

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
        ErlideTestUtils.initModulesAndIncludes();
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
        final ErlSearchScope scope = new ErlSearchScope();
        scope.addModule(moduleA);
        scope.addModule(moduleB);
        final ErlSearchQuery query = new ErlSearchQuery(ref, scope, "");
        query.run(new NullProgressMonitor());
        // then
        // it should be found in module b
        final ErlangSearchResult searchResult = (ErlangSearchResult) query
                .getSearchResult();
        assertEquals(nFoundExpected, searchResult.getMatchCount());
        final List<ErlangSearchElement> result = searchResult.getResult();
        // TODO check result
    }

    @Test
    public void findCallAfterRecordRef() throws Exception {
        // given
        // a module a with an exported function f
        // and a module b which calls a:f()
        final IErlModule moduleA = ErlideTestUtils.createModule(projects[0],
                "a.erl", "-module(a).\n-export([f/0]).\nf() ->\n    ok.\n");
        final IErlModule moduleB = ErlideTestUtils
                .createModule(projects[0], "b.erl",
                        "-module(b).\n-export([f/0]).\nf() ->\n    #a.b,\n    a:f().\n");
        moduleA.open(null);
        moduleB.open(null);
        // when
        // searching for the call to a:f
        final ErlangSearchPattern ref = ErlangSearchPattern.getSearchPattern(
                SearchFor.FUNCTION, "a", "f", 0, LimitTo.REFERENCES);
        final ErlSearchScope scope = new ErlSearchScope();
        scope.addModule(moduleA);
        scope.addModule(moduleB);
        final ErlSearchQuery query = new ErlSearchQuery(ref, scope, "");
        query.run(new NullProgressMonitor());
        // then
        // it should be found in module b
        final ErlangSearchResult searchResult = (ErlangSearchResult) query
                .getSearchResult();
        assertEquals(1, searchResult.getMatchCount());
        // final List<ErlangSearchElement> result = searchResult.getResult();
        // TODO check result

    }
}
