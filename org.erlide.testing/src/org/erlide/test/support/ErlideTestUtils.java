package org.erlide.test.support;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IOldErlangProjectProperties;
import org.erlide.core.preferences.OldErlangProjectProperties;
import org.erlide.core.text.ErlangToolkit;

import com.google.common.collect.Lists;

public class ErlideTestUtils {

	private static List<IErlModule> modules;
	private static List<IErlProject> projects;

	private static void buildPaths(final IWorkspaceRoot root,
			final IProject project, final Collection<IPath> list)
			throws CoreException {
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

	public static void initModules() {
		modules = Lists.newArrayList();
	}

	public static IErlModule createModule(final IErlProject erlProject,
			final String moduleName, final String moduleContents)
			throws CoreException {
		final IProject project = erlProject.getProject();
		final IFolder folder = project.getFolder("src");
		final IFile file = folder.getFile(moduleName);
		final File f = new File(file.getLocation().toOSString());
		f.delete();
		file.create(new ByteArrayInputStream(moduleContents.getBytes()), true,
				null);
		IErlModule module = ErlangCore.getModel().findModule(file);
		if (module == null) {
			final String path = file.getLocation().toPortableString();
			module = ErlangCore.getModelManager().getModuleFromFile(
					ErlangCore.getModel(), file.getName(), null, path, path);
		}
		modules.add(module);
		return module;
	}

	public static void deleteModule(final IErlModule module)
			throws CoreException {
		final String scannerName = ErlangToolkit
				.createScannerModuleName(module);
		final IFile file = (IFile) module.getResource();
		if (file != null) {
			file.delete(true, null);
		}
		final IPath stateDir = ErlangPlugin.getDefault().getStateLocation();
		final String cacheExts[] = { ".noparse", ".refs", ".scan" };
		for (final String ext : cacheExts) {
			final IPath p = stateDir.append(scannerName + ext);
			final File f = new File(p.toOSString());
			f.delete();
		}
		module.dispose();
		modules.remove(module);
	}

	public static IErlProject createProject(final IPath path, final String name)
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
		final IOldErlangProjectProperties prefs = new OldErlangProjectProperties(
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
		projects.add(erlProject);
		return erlProject;
	}

	private static void createFolderHelper(final IFolder folder)
			throws CoreException {
		if (!folder.exists()) {
			final IContainer parent = folder.getParent();
			if (parent instanceof IFolder) {
				createFolderHelper((IFolder) parent);
			}
			folder.create(false, true, null);
		}
	}

	public static IPath getTmpPath(final String fileName) {
		final String tmpdir = System.getProperty("java.io.tmpdir");
		return new Path(tmpdir).append(fileName);
	}

	public static File createTmpFile(final String fileName,
			final String contentString) throws IOException,
			FileNotFoundException {
		final String pathString = getTmpPath(fileName).toOSString();
		final File f = new File(pathString);
		f.createNewFile();
		final FileOutputStream fileOutputStream = new FileOutputStream(
				pathString);
		fileOutputStream.write(contentString.getBytes());
		fileOutputStream.close();
		return f;
	}

	public static void deleteProject(final IErlProject erlProject)
			throws CoreException {
		final IProject project = erlProject.getProject();
		project.delete(true, null);
		final List<IErlModule> list = Lists.newArrayList(modules);
		for (final IErlModule module : list) {
			if (module.getProject() == erlProject) {
				deleteModule(module);
			}
		}
		projects.remove(project);
		final IErlModel model = ErlangCore.getModel();
		model.resourceChanged(null);
		model.open(null);
	}

	public static void invokeBuilderOn(final IErlProject erlProject)
			throws CoreException {
		final IProject project = erlProject.getProject();
		project.build(IncrementalProjectBuilder.FULL_BUILD, null);
	}

	public static void deleteModules() throws CoreException {
		final List<IErlModule> list = Lists.newArrayList(modules);
		for (final IErlModule module : list) {
			deleteModule(module);
		}
	}

	public static void deleteProjects() throws CoreException {
		final List<IErlProject> list = Lists.newArrayList(projects);
		for (final IErlProject project : list) {
			deleteProject(project);
		}
	}

	public static void initProjects() {
		projects = Lists.newArrayList();
	}

	public static IErlModule createModuleFromText(final String initialText) {
		final IErlModule module = ErlangCore.getModelManager()
				.getModuleFromText(ErlangCore.getModel(), "test1", initialText,
						"test1");
		modules.add(module);
		return module;
	}

	public static IErlProject createTmpErlProject(final String projectName)
			throws CoreException {
		return createProject(getTmpPath(projectName), projectName);
	}

}
