package org.erlide.engine.util;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.filesystem.URIUtil;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IMarkerDelta;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlProject;

import com.google.common.collect.Lists;

public class ErlideTestUtils {

    // TODO replace ResourceDeltaStub with a mock object
    public static class ResourceDeltaStub implements IResourceDelta {
        @Override
        public Object getAdapter(final Class adapter) {
            return null;
        }

        @Override
        public void accept(final IResourceDeltaVisitor visitor) throws CoreException {
        }

        @Override
        public void accept(final IResourceDeltaVisitor visitor,
                final boolean includePhantoms) throws CoreException {
        }

        @Override
        public void accept(final IResourceDeltaVisitor visitor, final int memberFlags)
                throws CoreException {
        }

        @Override
        public IResourceDelta findMember(final IPath path) {
            return null;
        }

        @Override
        public IResourceDelta[] getAffectedChildren() {
            return null;
        }

        @Override
        public IResourceDelta[] getAffectedChildren(final int kindMask) {
            return null;
        }

        @Override
        public IResourceDelta[] getAffectedChildren(final int kindMask,
                final int memberFlags) {
            return null;
        }

        @Override
        public int getFlags() {
            return CONTENT;
        }

        @Override
        public IPath getFullPath() {
            return null;
        }

        @Override
        public int getKind() {
            return 0;
        }

        @Override
        public IMarkerDelta[] getMarkerDeltas() {
            return null;
        }

        @Override
        public IPath getMovedFromPath() {
            return null;
        }

        @Override
        public IPath getMovedToPath() {
            return null;
        }

        @Override
        public IPath getProjectRelativePath() {
            return null;
        }

        @Override
        public IResource getResource() {
            return null;
        }
    }

    private static List<IErlModule> modulesAndIncludes;
    private static List<IErlProject> projects;

    private static void buildPaths(final IWorkspaceRoot root, final IProject project,
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

    public static void initModulesAndIncludes() {
        modulesAndIncludes = Lists.newArrayList();
    }

    public static IErlModule createModule(final IErlProject project,
            final String moduleName, final String moduleContents) throws CoreException {
        final IFolder folder = project.getWorkspaceProject().getFolder("src");
        final IErlModule module = createModule(moduleName, moduleContents, folder);
        modulesAndIncludes.add(module);
        return module;
    }

    public static IErlModule createInclude(final IErlProject project,
            final String moduleName, final String moduleContents) throws CoreException {
        final IFolder folder = project.getWorkspaceProject().getFolder("include");
        final IErlModule module = createModule(moduleName, moduleContents, folder);
        modulesAndIncludes.add(module);
        return module;
    }

    public static IErlModule createModule(final String moduleName,
            final String moduleContents, final IFolder folder) throws CoreException {
        final IFile file = createFile(moduleName, moduleContents, folder);
        final IErlModel model = ErlangEngine.getInstance().getModel();
        IErlModule module = model.findModule(file);
        if (module == null) {
            final String path = file.getLocation().toPortableString();
            module = model.getModuleFromFile(model, file.getName(), path, Charset
                    .defaultCharset().name(), path);
        }
        return module;
    }

    public static IFile createFile(final String name, final String contents,
            final IFolder folder) throws CoreException {
        final IFile file = folder.getFile(name);
        final File f = new File(file.getLocation().toOSString());
        f.delete();
        file.create(
                new ByteArrayInputStream(contents.getBytes(Charset.defaultCharset())),
                true, null);
        return file;
    }

    public static void deleteModule(final IErlModule module) throws CoreException {
        final String scannerName = module.getScannerName();
        final IFile file = (IFile) module.getResource();
        if (file != null) {
            file.delete(true, null);
        }
        final IPath stateDir = new Path(ErlangEngine.getInstance().getStateDir());
        // FIXME this code should not know about caches!
        final String cacheExts[] = { ".noparse", ".refs", ".scan" };
        for (final String ext : cacheExts) {
            final IPath p = stateDir.append(scannerName + ext);
            final File f = new File(p.toOSString());
            f.delete();
        }
        module.dispose();
        modulesAndIncludes.remove(module);
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
        final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                .newProject(name, path.toPortableString());
        erlProject.getBuilderProperties().setBuilderTool(BuilderTool.INTERNAL);

        final IProject project = erlProject.getWorkspaceProject();
        final ErlangProjectProperties prefs = erlProject.getProperties();

        final List<IPath> srcDirs = new ArrayList<IPath>();
        srcDirs.add(new Path("src"));
        prefs.setSourceDirs(srcDirs);
        buildPaths(root, project, srcDirs);

        final List<IPath> includeDirs = new ArrayList<IPath>();
        includeDirs.add(new Path("include"));
        buildPaths(root, project, includeDirs);
        prefs.setIncludeDirs(includeDirs);

        final IPath ebinDir = new Path("ebin");
        buildPaths(root, project, Lists.newArrayList(ebinDir));
        prefs.setOutputDir(ebinDir);

        projects.add(erlProject);
        return erlProject;
    }

    public static IErlProject getExistingProject(final String name) {
        final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        final IProject project = root.getProject(name);
        return ErlangEngine.getInstance().getModel().getErlangProject(project);
    }

    public static void createFolderHelper(final IFolder folder) throws CoreException {
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

    public static URI getTmpURIPath(final String fileName) {
        return URIUtil.toURI(getTmpPath(fileName).toPortableString());
    }

    public static File createTmpFile(final String fileName, final String contentString)
            throws IOException, FileNotFoundException {
        final String pathString = getTmpPath(fileName).toOSString();
        final File f = new File(pathString);
        if (f.exists()) {
            f.delete();
        }
        f.createNewFile();
        final FileOutputStream fileOutputStream = new FileOutputStream(pathString);
        fileOutputStream.write(contentString.getBytes());
        fileOutputStream.close();
        return f;
    }

    public static void deleteProject(final IErlProject erlProject) throws CoreException {
        final IProject project = erlProject.getWorkspaceProject();
        final IPath location = project.getLocation();
        project.delete(true, null);
        if (location != null) {
            new File(location.toPortableString()).delete();
        }
        if (modulesAndIncludes != null) {
            final List<IErlModule> list = Lists.newArrayList(modulesAndIncludes);
            for (final IErlModule module : list) {
                if (ErlangEngine.getInstance().getModelUtilService().getProject(module) == erlProject) {
                    deleteModule(module);
                }
            }
        }
        erlProject.dispose();
        if (projects != null) {
            projects.remove(ErlangEngine.getInstance().getModel().findProject(project));
        }
        final IErlModel model = ErlangEngine.getInstance().getModel();
        model.resourceChanged(null);
        model.open(null);
    }

    public static void invokeBuilderOn(final IErlProject erlProject) throws CoreException {
        final IProject project = erlProject.getWorkspaceProject();
        project.build(IncrementalProjectBuilder.FULL_BUILD, null);
    }

    public static void deleteModules() throws CoreException {
        final List<IErlModule> list = Lists.newArrayList(modulesAndIncludes);
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

    public static void initProjects() throws CoreException {
        projects = Lists.newArrayList();
        final IErlModel model = ErlangEngine.getInstance().getModel();
        model.open(null);
        final List<IErlElement> children = model.getChildren();
        for (final IErlElement child : children) {
            if (child instanceof IErlProject) {
                final IErlProject project = (IErlProject) child;
                if (project.getName().startsWith("testproject")) {
                    deleteProject(project);
                }
            }
        }
    }

    public static IErlModule createModuleFromText(final String initialText) {
        final IErlModel model = ErlangEngine.getInstance().getModel();
        final IErlModule module = model.getModuleFromText(model, "test1", initialText,
                "test1");
        modulesAndIncludes.add(module);
        return module;
    }

    public static IErlProject createTmpErlProject(final String projectName)
            throws CoreException {
        return createProject(getTmpPath(projectName), projectName);
    }

    public static IPath[] splitPathAfter(final int i, final IPath p) {
        final IPath last = p.removeFirstSegments(i);
        final IPath first = p.removeLastSegments(p.segmentCount() - i);
        return new IPath[] { first, last };
    }

    public static void refreshProjects() {
        for (final IErlProject project : projects) {
            project.resourceChanged(new ResourceDeltaStub());
        }
    }

}
