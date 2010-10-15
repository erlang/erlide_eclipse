package org.erlide.core.builder;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.util.ResourceUtil;
import org.erlide.test.support.ErlideTestUtils;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class DialyzerUtilsTest {

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
	}

	@Before
	public void setUp() throws Exception {
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void dialyzePrepareSelectionBeamsTest() throws Exception {
		dialyzePrepareFromSelection(false);
	}

	@Test
	public void dialyzePrepareSelectionSourcesTest() throws Exception {
		dialyzePrepareFromSelection(true);
	}

	@Test
	public void dialyzeMarkerOnfile() throws Exception {
		IErlProject erlProject = null;
		try {
			// given
			// an erlang module in an erlang project
			final String projectName = "testproject";
			erlProject = ErlideTestUtils.createErlProject(
					ErlideTestUtils.getTmpPath(projectName), projectName);
			final String moduleName = "test.erl";
			final IErlModule erlModule = ErlideTestUtils
					.createErlModule(erlProject, moduleName,
							"-module(test).\n-export([f/0]).\n-f() ->\n    atom_to_list(\"hej\").\n");
			IMarker[] markers = erlProject.getProject().findMarkers(
					DialyzerUtils.DIALYZE_WARNING_MARKER, true,
					IResource.DEPTH_INFINITE);
			assertEquals(0, markers.length);
			// when
			// putting a dialyzer warning on it
			final int lineNumber = 3;
			final String message = "test message";
			DialyzerUtils.addDialyzerWarningMarker(erlProject.getProject(),
					erlModule.getResource().getLocation().toPortableString(),
					lineNumber, message);
			// then
			// there should be a marker with proper file name and the proper
			// line number
			markers = erlProject.getProject().findMarkers(
					DialyzerUtils.DIALYZE_WARNING_MARKER, true,
					IResource.DEPTH_INFINITE);
			assertEquals(1, markers.length);
			final IMarker marker = markers[0];
			assertEquals(moduleName, marker.getResource().getName());
			assertEquals(lineNumber, marker.getAttribute(IMarker.LINE_NUMBER));
			assertEquals(message, marker.getAttribute(IMarker.MESSAGE));
		} finally {
			if (erlProject != null) {
				ErlideTestUtils.deleteErlProject(erlProject);
			}
		}
	}

	@Test
	public void dialyzeWithExternalInclude() throws Exception {
		// http://www.assembla.com/spaces/erlide/tickets/608-dialyzer---navigate-to-external-includes-from-markers
		File externalFile = null;
		IErlProject erlProject = null;
		try {
			// given
			// an erlang project and an external file not in any project
			final String projectName = "testproject";
			erlProject = ErlideTestUtils.createErlProject(
					ErlideTestUtils.getTmpPath(projectName), projectName);
			final String externalFileName = "external.hrl";
			externalFile = ErlideTestUtils.createTmpFile(externalFileName,
					"f([_ | _]=L ->\n    atom_to_list(L).\n");
			// when
			// putting dialyzer warning markers on the external file
			final String message = "test message";
			final int lineNumber = 2;
			DialyzerUtils.addDialyzerWarningMarker(erlProject.getProject(),
					externalFile.getAbsolutePath(), lineNumber, message);
			// then
			// the marker should have the proper file name and the include file
			// should appear in External Files
			final IProject externalFilesProject = ResourceUtil
					.getExternalFilesProject();
			final IFile file = externalFilesProject.getFile(new Path(
					externalFileName));
			assertNotNull(file);
			final IWorkspaceRoot root = ResourcesPlugin.getWorkspace()
					.getRoot();
			final IMarker[] markers = root.getProject("External_Files")
					.findMarkers(DialyzerUtils.DIALYZE_WARNING_MARKER, true,
							IResource.DEPTH_INFINITE);
			assertTrue(markers.length > 0);
			for (final IMarker marker : markers) {
				// for some reason, when running on Hudson, we get two identical
				// markers...
				assertEquals(externalFileName, marker.getResource().getName());
				assertEquals(lineNumber,
						marker.getAttribute(IMarker.LINE_NUMBER));
				assertEquals(message, marker.getAttribute(IMarker.MESSAGE));
			}
		} finally {
			if (externalFile != null && externalFile.exists()) {
				externalFile.delete();
			}
			if (erlProject != null) {
				ErlideTestUtils.deleteErlProject(erlProject);
			}
		}
	}

	public void dialyzePrepareFromSelection(final boolean sources)
			throws Exception {
		// http://www.assembla.com/spaces/erlide/tickets/607-dialyzer---only-dialyze-on-selection
		IErlProject erlProject = null;
		try {
			// given
			// a project with two erlang modules, one of them selected
			final String projectName = "testproject";
			erlProject = ErlideTestUtils.createErlProject(
					ErlideTestUtils.getTmpPath(projectName), projectName);
			assertNotNull(erlProject);
			final IErlModule a = ErlideTestUtils
					.createErlModule(
							erlProject,
							"a.erl",
							"-module(a).\n-export([t/0]).\nt() ->\n    p(a).\np(L) ->\n    lists:reverse(L).\n");
			assertNotNull(a);
			final IErlModule b = ErlideTestUtils
					.createErlModule(
							erlProject,
							"b.erl",
							"-module(b).\n-export([t/0]).\nt() ->\n    p(a).\np(L) ->\n    lists:reverse(L).\n");
			assertNotNull(b);
			ErlideTestUtils.invokeBuilderOn(erlProject);
			// when
			// collecting files to dialyze
			final Set<IErlModule> selectedModules = new HashSet<IErlModule>();
			selectedModules.add(a);
			final Map<IErlProject, Set<IErlModule>> modules = new HashMap<IErlProject, Set<IErlModule>>();
			modules.put(erlProject, selectedModules);
			final List<String> names = new ArrayList<String>();
			final List<IPath> includeDirs = new ArrayList<IPath>();
			final List<String> files = new ArrayList<String>();
			DialyzerUtils
					.collectFilesAndIncludeDirs(erlProject, modules,
							erlProject.getProject(), files, names, includeDirs,
							sources);
			// then
			// only selected files (or corresponding beam) should be collected
			assertEquals(1, files.size());
			final IPath p = new Path(files.get(0));
			final String f = p.lastSegment();
			if (sources) {
				assertEquals("a.erl", f);
			} else {
				assertEquals("a.beam", f);
			}

		} finally {
			if (erlProject != null) {
				ErlideTestUtils.deleteErlProject(erlProject);
			}
		}
	}
}
