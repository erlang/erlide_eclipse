package org.erlide.core.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class ErlProjectTest {

    private static IErlProject[] projects;

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
        ErlideTestUtils.initModulesAndHeaders();
    }

    @After
    public void tearDown() throws Exception {
        ErlideTestUtils.deleteModules();
    }

    @Test
    public void findIncludeFile() throws Exception {
        // given
        // a project with a module and an include including file.hrl
        final IErlProject project = projects[0];
        final String headerName = "a.hrl";
        final IErlModule header = ErlideTestUtils
                .createModule(
                        project,
                        headerName,
                        "-include_lib(\"kernel/include/file.hrl\").\n-record(rec1, {field, another=def}).\n-define(MACRO(A), lists:reverse(A)).\n");
        header.open(null);
        final IErlModule module = ErlideTestUtils
                .createModule(
                        project,
                        "f.erl",
                        "-module(f).\n-include(\"a.hrl\").\n-export([f/0]).\n-record(rec2, {a, b}).\n"
                                + "f() ->\n    lists:reverse([1, 0]),\n    lists:reverse([1, 0], [2]).\n");
        module.open(null);
        // when
        // looking for the include
        final IErlModule header1 = project.findIncludeFile(headerName);
        final IErlModule header2 = project.findIncludeFile("file.hrl");
        // then
        // it should be found
        assertEquals(header, header1);
        assertNotNull(header2);
    }

}
