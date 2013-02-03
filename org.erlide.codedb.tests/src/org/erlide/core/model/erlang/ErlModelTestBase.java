package org.erlide.core.model.erlang;

import org.eclipse.core.runtime.CoreException;
import org.erlide.core.model.root.IErlProject;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;

public class ErlModelTestBase {

    protected static IErlProject[] projects;

    protected static void setupProjects() throws CoreException {
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

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        setupProjects();
    }

    @AfterClass
    public static void tearDownAfterClass() throws Exception {
        ErlideTestUtils.deleteProjects();
    }

    protected IErlModule module;
    protected IErlProject project;

    public void setupModules() throws CoreException {
        ErlideTestUtils.initModulesAndIncludes();
        project = projects[0];
        module = ErlideTestUtils.createModule(projects[0], "xx.erl",
                "-module(xx).\n-include(\"yy.hrl\").\n"
                        + "f(A) ->\n    lists:reverse(A).\n");
    }

    protected void tearDownModules() throws CoreException {
        ErlideTestUtils.deleteModules();
        ErlideTestUtils.refreshProjects();
    }

    @Before
    public void setUp() throws Exception {
        setupModules();
    }

    @After
    public void tearDown() throws Exception {
        tearDownModules();
    }

}
