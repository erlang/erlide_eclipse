package org.erlide.core.builder;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.ByteArrayInputStream;
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
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
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

	public void dialyzePrepareFromSelection(final boolean sources)
			throws Exception {
		IErlProject erlProject = null;
		try {
			// given
			// a project with two erlang modules, one of them selected
			final String projectName = "testproject";
			final String tmpdir = System.getProperty("java.io.tmpdir");
			erlProject = createErlProject(new Path(tmpdir).append(projectName),
					projectName);
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

	private IErlProject createErlProject(final IPath filePath, final String name)
			throws CoreException {
		final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		final IProject project2 = root.getProject(name);
		try {
			project2.delete(true, null);
		} catch (final CoreException x) {
		}
		final IProject project = root.getProject(name);
		IProjectDescription description = ResourcesPlugin.getWorkspace()
				.newProjectDescription(project.getName());
		if (!Platform.getLocation().equals(filePath)) {
			description.setLocation(filePath);
		}
		project.create(description, null);
		project.open(null);
		description = project.getDescription();
		description.setNatureIds(new String[] { ErlangPlugin.NATURE_ID });
		project.setDescription(description, null);
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
		final IErlProject erlProject = (IErlProject) ErlangCore
				.getModelManager().create(project, null);
		return erlProject;
	}

	private void deleteErlProject(final IErlProject erlProject)
			throws CoreException {
		final IProject project = erlProject.getProject();
		project.delete(true, null);
	}
}
