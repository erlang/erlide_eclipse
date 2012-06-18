package org.erlide.core.services.builder;

import static org.junit.Assert.*;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlElementLocator;
import org.erlide.core.model.root.IErlProject;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class DialyzerUtilsTest {

    enum SEL {
        MODULE, SRC, PROJECT
    }

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        ErlideTestUtils.initProjects();
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
    public void dialyzePrepareSelectionModuleBeamsTest() throws Exception {
        dialyzePrepareFromSelection(false, SEL.MODULE);
    }

    @Test
    public void dialyzePrepareSelectionModuleSourcesTest() throws Exception {
        dialyzePrepareFromSelection(true, SEL.MODULE);
    }

    @Test
    public void dialyzePrepareSelectionSrcFolderBeamsTest() throws Exception {
        dialyzePrepareFromSelection(false, SEL.SRC);
    }

    @Test
    public void dialyzePrepareSelectionSrcFolderSourcesTest() throws Exception {
        dialyzePrepareFromSelection(true, SEL.SRC);
    }

    @Test
    public void dialyzePrepareSelectionProjectBeamsTest() throws Exception {
        dialyzePrepareFromSelection(false, SEL.PROJECT);
    }

    @Test
    public void dialyzePrepareSelectionProjectSourcesTest() throws Exception {
        dialyzePrepareFromSelection(true, SEL.PROJECT);
    }

    @Test
    public void dialyzeMarkerOnfile() throws Exception {
        IErlProject erlProject = null;
        try {
            // given
            // an erlang module in an erlang project
            final String projectName = "testproject";
            erlProject = ErlideTestUtils.createTmpErlProject(projectName);
            final String moduleName = "test.erl";
            final IErlModule erlModule = ErlideTestUtils
                    .createModule(erlProject, moduleName,
                            "-module(test).\n-export([f/0]).\n-f() ->\n    atom_to_list(\"hej\").\n");
            IMarker[] markers = erlProject.getWorkspaceProject().findMarkers(
                    MarkerUtils.DIALYZE_WARNING_MARKER, true,
                    IResource.DEPTH_INFINITE);
            assertEquals(0, markers.length);
            // when
            // putting a dialyzer warning on it
            final int lineNumber = 3;
            final String message = "test message";
            final IErlElementLocator model = erlProject.getModel();
            MarkerUtils.addDialyzerWarningMarker(model, erlModule.getResource()
                    .getLocation().toPortableString(), lineNumber, message);
            // then
            // there should be a marker with proper file name and the proper
            // line number
            markers = erlProject.getWorkspaceProject().findMarkers(
                    MarkerUtils.DIALYZE_WARNING_MARKER, true,
                    IResource.DEPTH_INFINITE);
            assertEquals(1, markers.length);
            final IMarker marker = markers[0];
            assertEquals(moduleName, marker.getResource().getName());
            assertEquals(lineNumber, marker.getAttribute(IMarker.LINE_NUMBER));
            assertEquals(message, marker.getAttribute(IMarker.MESSAGE));
        } finally {
            if (erlProject != null) {
                ErlideTestUtils.deleteProject(erlProject);
            }
        }
    }

    @Test
    public void dialyzeWithExternalInclude() throws Exception {
        // http://www.assembla.com/spaces/erlide/tickets/608-dialyzer---navigate-to-external-includes-from-markers
        File externalFile = null;
        IErlProject erlProject = null;
        File externalInclude = null;
        final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        try {
            // given
            // an erlang project and an external file not in any project
            final String projectName = "testproject";
            erlProject = ErlideTestUtils.createTmpErlProject(projectName);
            final String externalFileName = "external.hrl";
            externalFile = ErlideTestUtils.createTmpFile(externalFileName,
                    "f([_ | _]=L ->\n    atom_to_list(L).\n");
            externalInclude = ErlideTestUtils.createTmpFile(
                    "external_includes", externalFile.getAbsolutePath());
            MarkerUtils.removeDialyzerMarkers(root);
            // when
            // putting dialyzer warning markers on the external file
            final String message = "test message";
            final int lineNumber = 2;
            final IErlElementLocator model = erlProject.getModel();
            MarkerUtils.addDialyzerWarningMarker(model,
                    externalFile.getAbsolutePath(), lineNumber, message);
            // then
            // the marker should have the proper file name and the include file
            // should appear in External Files
            final IMarker[] markers = root.findMarkers(
                    MarkerUtils.DIALYZE_WARNING_MARKER, true,
                    IResource.DEPTH_INFINITE);
            // FIXME this fails on Hudson!
            // assertTrue(markers.length > 0);
            for (final IMarker marker : markers) {
                // for some reason, when running on Hudson, we get two identical
                // markers...
                final String path = (String) marker
                        .getAttribute(MarkerUtils.PATH_ATTRIBUTE);
                final IPath p = new Path(path);
                assertEquals(externalFileName, p.lastSegment());
                assertEquals(lineNumber,
                        marker.getAttribute(IMarker.LINE_NUMBER));
                assertEquals(message, marker.getAttribute(IMarker.MESSAGE));
            }
        } finally {
            MarkerUtils.removeDialyzerMarkers(root);
            if (externalInclude != null && externalInclude.exists()) {
                externalInclude.delete();
            }
            if (externalFile != null && externalFile.exists()) {
                externalFile.delete();
            }
            if (erlProject != null) {
                ErlideTestUtils.deleteProject(erlProject);
            }
        }
    }

    public void dialyzePrepareFromSelection(final boolean sources,
            final SEL select) throws Exception {
        // http://www.assembla.com/spaces/erlide/tickets/607-dialyzer---only-dialyze-on-selection
        IErlProject erlProject = null;
        try {
            // given
            // a project with two erlang modules, one of them selected
            final String projectName = "testproject";
            erlProject = ErlideTestUtils.createTmpErlProject(projectName);
            assertNotNull(erlProject);
            final IErlModule a = ErlideTestUtils
                    .createModule(
                            erlProject,
                            "a.erl",
                            "-module(a).\n-export([t/0]).\nt() ->\n    p(a).\np(L) ->\n    lists:reverse(L).\n");
            assertNotNull(a);
            final IErlModule b = ErlideTestUtils
                    .createModule(
                            erlProject,
                            "b.erl",
                            "-module(b).\n-export([t/0]).\nt() ->\n    p(a).\np(L) ->\n    lists:reverse(L).\n");
            assertNotNull(b);
            ErlideTestUtils.invokeBuilderOn(erlProject);
            // when
            // collecting files to dialyze
            final Map<IErlProject, Set<IErlModule>> modules = new HashMap<IErlProject, Set<IErlModule>>();
            final IResource selectedResource = selectResource(select,
                    erlProject, a);
            DialyzerUtils
                    .addModulesFromResource(ErlModelManager.getErlangModel(),
                            selectedResource, modules);
            final List<String> names = new ArrayList<String>();
            final List<IPath> includeDirs = new ArrayList<IPath>();
            final List<String> files = new ArrayList<String>();
            DialyzerUtils.collectFilesAndIncludeDirs(erlProject, modules,
                    erlProject.getWorkspaceProject(), files, names,
                    includeDirs, sources);
            // then
            // only selected files (or corresponding beam) should be collected
            if (select == SEL.MODULE) {
                assertEquals(1, files.size());
                final IPath p = new Path(files.get(0));
                final String f = p.lastSegment();
                if (sources) {
                    assertEquals("a.erl", f);
                } else {
                    assertEquals("a.beam", f);
                }
            } else {
                assertEquals(2, files.size());
                final Set<String> fSet = new HashSet<String>(2);
                for (final String i : files) {
                    fSet.add(new Path(i).lastSegment());
                }
                if (sources) {
                    assertTrue(fSet.contains("a.erl"));
                    assertTrue(fSet.contains("b.erl"));
                } else {
                    assertTrue(fSet.contains("a.beam"));
                    assertTrue(fSet.contains("b.beam"));
                }
            }

        } finally {
            if (erlProject != null) {
                ErlideTestUtils.deleteProject(erlProject);
            }
        }
    }

    private IResource selectResource(final SEL select,
            final IErlProject erlProject, final IErlModule a) {
        switch (select) {
        case MODULE:
            return a.getResource();
        case PROJECT:
            return erlProject.getResource();
        default:
        case SRC:
            return erlProject.getWorkspaceProject().getFolder("src");
        }
    }

    // @Test
    // public void dialyzeModuleWithExternalInclude() throws Exception {
    // IErlProject erlProject = null;
    // try {
    // // given
    // // a project with an erlang module, inluding an external file
    // final String projectName = "testproject";
    // erlProject = createTmpErlProject(projectName);
    // ErlideTestUtils.getTmpPath("testexternals");
    // assertNotNull(erlProject);
    // final IErlModule include = ErlideTestUtils.createErlModule(
    // erlProject, "i.hrl", "-record(a, {b, c}).\n");
    // final IErlModule f = ErlideTestUtils
    // .createErlModule(
    // erlProject,
    // "f.erl",
    // "-module(a).\n-export([t/0]).\n-include(\"i.hrl\").\nt() ->\n    p(#a{b=b, c=c}).\n");
    // assertNotNull(f);
    // ErlideTestUtils.invokeBuilderOn(erlProject);
    // // when
    // // dialyzing it
    // final Map<IErlProject, Set<IErlModule>> modules = new
    // HashMap<IErlProject, Set<IErlModule>>();
    // DialyzerUtils.addModulesFromResource(ErlangCore.getModel(),
    // erlProject.getResource(), modules);
    // final List<String> names = new ArrayList<String>();
    // final List<IPath> includeDirs = new ArrayList<IPath>();
    // final List<String> files = new ArrayList<String>();
    // DialyzerUtils.collectFilesAndIncludeDirs(erlProject, modules,
    // erlProject.getProject(), files, names, includeDirs, false);
    // // then
    // // it should find the include file
    // assertEquals(1, files.size());
    // assertEquals("a.beam", new Path(files.get(0)).lastSegment());
    //
    // } finally {
    // if (erlProject != null) {
    // ErlideTestUtils.deleteErlProject(erlProject);
    // }
    // }

    @Test
    public void dialyzeBinaryOnProjectWithErrorFile() throws Exception {
        // http://www.assembla.com/spaces/erlide/tickets/616-dialyzer-�-crash-on-binary-analysis-and-files-with-errors
        IErlProject erlProject = null;
        try {
            // given
            // a project with two erlang modules, one of them with an erlang
            // error, preventing it from generating a beam-file
            final String projectName = "testproject";
            erlProject = ErlideTestUtils.createTmpErlProject(projectName);
            assertNotNull(erlProject);
            final IErlModule a = ErlideTestUtils
                    .createModule(
                            erlProject,
                            "a.erl",
                            "-module(a).\n-export([t/0]).\nt() ->\n    p(a).\np(L) ->\n    lists:reverse(L).\n");
            assertNotNull(a);
            final IErlModule b = ErlideTestUtils
                    .createModule(
                            erlProject,
                            "b.erl",
                            "-module(b).\n-export([t/0]).\nt() ->\n    p(a).\np(L) ->\n    fel som tusan.\n");
            assertNotNull(b);
            ErlideTestUtils.invokeBuilderOn(erlProject);
            // when
            // collecting files to dialyze
            final Map<IErlProject, Set<IErlModule>> modules = new HashMap<IErlProject, Set<IErlModule>>();
            DialyzerUtils.addModulesFromResource(
                    ErlModelManager.getErlangModel(), erlProject.getResource(),
                    modules);
            final List<String> names = new ArrayList<String>();
            final List<IPath> includeDirs = new ArrayList<IPath>();
            final List<String> files = new ArrayList<String>();
            DialyzerUtils.collectFilesAndIncludeDirs(erlProject, modules,
                    erlProject.getWorkspaceProject(), files, names,
                    includeDirs, false);
            // then
            // it should only take the existing beam files
            assertEquals(1, files.size());
            final IPath p = new Path(files.get(0));
            final String f = p.lastSegment();
            assertEquals("a.beam", f);

        } finally {
            if (erlProject != null) {
                ErlideTestUtils.deleteProject(erlProject);
            }
        }
    }
}
