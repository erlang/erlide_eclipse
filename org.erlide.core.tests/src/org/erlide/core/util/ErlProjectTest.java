package org.erlide.core.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.backend.ErlCallable;
import org.erlide.backend.util.StringUtils;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.util.ModelUtils;
import org.erlide.core.text.ErlangToolkit;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.google.common.collect.Lists;

import erlang.ErlideOpen;
import erlang.OpenResult;

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

    @Test
    public void findExternalIncludeFileOnIncludePath() throws Exception {
        File externalHeader = null;
        IErlProject project = null;
        // given
        // a project with an include dir outside the model
        try {
            final String projectName = "testprojectx";
            project = ErlideTestUtils.createProject(
                    ErlideTestUtils.getTmpPath(projectName), projectName);
            final String headerName = "x.hrl";
            externalHeader = ErlideTestUtils.createTmpFile(headerName,
                    "-record(rec2, {field, another=def}.");
            final String headerPath = externalHeader.getAbsolutePath();
            final IPath p = new Path(headerPath).removeLastSegments(1);
            project.setIncludeDirs(Lists.newArrayList(p));
            // when
            // looking for the include file
            // String includeFile = ModelUtils.findIncludeFile(erlProject,
            // "x.hrl", "");
            project.open(null);
            final IErlModule module = project.findExternalModule(null,
                    headerPath, true, false);
            final IErlModule module2 = project.findExternalModule(headerName,
                    null, true, false);
            // then
            // it should be found in the model
            assertNotNull(module);
            assertEquals(module, module2);
        } finally {
            if (project != null) {
                ErlideTestUtils.deleteProject(project);
            }
            if (externalHeader != null && externalHeader.exists()) {
                externalHeader.delete();
            }
        }
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
        final ErlCallable backend = ErlangCore.getBackendManager().getIdeBackend();
        final IErlModel model = ErlangCore.getModel();
        final OpenResult res = ErlideOpen.open(backend,
                ErlangToolkit.createScannerModuleName(moduleE), 49,
                ModelUtils.getImportsAsList(moduleE),
                project.getExternalModulesString(), model.getPathVars());
        final IErlElement function = ModelUtils.findFunction(res.getName(),
                res.getFunction(), res.getPath(), project, false, moduleE);
        final IErlElement module = project.findExternalModule(
                function.getModuleName(), res.getPath(), false, false);
        // then
        // the function should be returned and the module, in External Files
        assertNotNull(function);
        assertTrue(function instanceof IErlFunction);
        assertNotNull(module);
        assertEquals(function.getParent(), module);
        assertEquals(function.getModule().getProject(), project);
    }

    @Test
    public void findExternalModule() throws Exception {
        File externalFile = null;
        IErlProject project = null;
        try {
            // given
            // an erlang project with an external file
            final String projectName = "testproject";
            project = ErlideTestUtils.createTmpErlProject(projectName);
            final String externalFileName = "external.erl";
            externalFile = ErlideTestUtils
                    .createTmpFile(externalFileName,
                            "-module(external).\nf([_ | _]=L ->\n    atom_to_list(L).\n");
            final String absolutePath = externalFile.getAbsolutePath();
            final String externalsFileName = "x.erlidex";
            final File externalsFile = ErlideTestUtils.createTmpFile(
                    externalsFileName, absolutePath);
            project.setExternalModulesFile(externalsFile.getAbsolutePath());
            project.open(null);
            // when
            // looking for it
            final IErlModule externalModule = project.findExternalModule(
                    externalFileName, null, true, false);
            // then
            // we should find it
            assertNotNull(externalModule);
            assertTrue(StringUtils.equalFilePaths(absolutePath,
                    externalModule.getFilePath()));
        } finally {
            if (externalFile != null && externalFile.exists()) {
                externalFile.delete();
            }
            if (project != null) {
                ErlideTestUtils.deleteProject(project);
            }
        }

    }

}
