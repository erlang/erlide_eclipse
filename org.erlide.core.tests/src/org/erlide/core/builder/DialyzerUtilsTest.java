package org.erlide.core.builder;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.util.ResourceUtil;
import org.erlide.core.preferences.OldErlangProjectProperties;
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
			erlProject = createErlProject(getTmpPath(projectName), projectName);
			final String moduleName = "test.erl";
			final IErlModule erlModule = createErlModule(erlProject,
					moduleName,
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
				deleteErlProject(erlProject);
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
			erlProject = createErlProject(getTmpPath(projectName), projectName);
			final String externalFileName = "external.hrl";
			externalFile = createTmpFile(externalFileName,
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
				deleteErlProject(erlProject);
			}
		}
	}

	private IPath getTmpPath(final String fileName) {
		final String tmpdir = System.getProperty("java.io.tmpdir");
		return new Path(tmpdir).append(fileName);
	}

	private File createTmpFile(final String fileName, final String contentString)
			throws IOException, FileNotFoundException {
		final String pathString = getTmpPath(fileName).toOSString();
		final File f = new File(pathString);
		f.createNewFile();
		final FileOutputStream fileOutputStream = new FileOutputStream(
				pathString);
		fileOutputStream.write(contentString.getBytes());
		fileOutputStream.close();
		return f;
	}

	public void dialyzePrepareFromSelection(final boolean sources)
			throws Exception {
		// http://www.assembla.com/spaces/erlide/tickets/607-dialyzer---only-dialyze-on-selection
		IErlProject erlProject = null;
		try {
			// given
			// a project with two erlang modules, one of them selected
			final String projectName = "testproject";
			erlProject = createErlProject(getTmpPath(projectName), projectName);
			assertNotNull(erlProject);
			final IErlModule a = createErlModule(
					erlProject,
					"a.erl",
					"-module(a).\n-export([t/0]).\nt() ->\n    p(a).\np(L) ->\n    lists:reverse(L).\n");
			assertNotNull(a);
			final IErlModule b = createErlModule(
					erlProject,
					"b.erl",
					"-module(b).\n-export([t/0]).\nt() ->\n    p(a).\np(L) ->\n    lists:reverse(L).\n");
			assertNotNull(b);
			invokeBuilderOn(erlProject);
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
				deleteErlProject(erlProject);
			}
		}
	}

	private void invokeBuilderOn(final IErlProject erlProject)
			throws CoreException {
		final IProject project = erlProject.getProject();
		project.build(IncrementalProjectBuilder.FULL_BUILD, null);
	}

	private IErlModule createErlModule(final IErlProject erlProject,
			final String moduleName, final String moduleContents)
			throws CoreException {
		final IProject project = erlProject.getProject();
		final IFolder folder = project.getFolder("src");
		final IFile file = folder.getFile(moduleName);
		file.create(new ByteArrayInputStream(moduleContents.getBytes()), true,
				null);
		return ErlangCore.getModel().findModule(file);
	}

	private void createFolderHelper(final IFolder folder) throws CoreException {
		if (!folder.exists()) {
			final IContainer parent = folder.getParent();
			if (parent instanceof IFolder) {
				createFolderHelper((IFolder) parent);
			}
			folder.create(false, true, null);
		}
	}

	private void buildPaths(final IWorkspaceRoot root, final IProject project,
			final Collection<IPath> list) throws CoreException {
		final IPath projectPath = project.getFullPath();
		for (final IPath pp : list) {
			// only create in-project paths
			if (!pp.isAbsolute() && !pp.toString().equals(".") && !pp.isEmpty()) {
				final IPath path = projectPath.append(pp);
				final IFolder folder = root.getFolder(path);
				createFolderHelper(folder);
			}
		}
	}

	private IErlProject createErlProject(final IPath path, final String name)
			throws CoreException {
		final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		final IProject project2 = root.getProject(name);
		try {
			project2.delete(true, null);
		} catch (final CoreException x) {
			// ignore
		}
		final IErlProject erlProject = ErlangCore.getModel().newProject(name,
				path.toPortableString());
		final IProject project = erlProject.getProject();
		final OldErlangProjectProperties prefs = new OldErlangProjectProperties(
				project);
		final List<IPath> srcDirs = new ArrayList<IPath>();
		srcDirs.add(new Path("src"));
		prefs.setSourceDirs(srcDirs);
		buildPaths(root, project, srcDirs);
		final List<IPath> includeDirs = new ArrayList<IPath>();
		includeDirs.add(new Path("include"));
		buildPaths(root, project, includeDirs);
		prefs.setIncludeDirs(includeDirs);
		final List<IPath> ebinDirs = new ArrayList<IPath>();
		ebinDirs.add(new Path("ebin"));
		buildPaths(root, project, ebinDirs);
		prefs.setOutputDir(ebinDirs.get(0));
		return erlProject;
	}

	private void deleteErlProject(final IErlProject erlProject)
			throws CoreException {
		final IProject project = erlProject.getProject();
		project.delete(true, null);
	}
}
