package org.erlide.testing.utils;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.InputStream;
import java.net.URL;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.IErlElement;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlFolder;
import org.erlide.engine.model.root.IErlModel;
import org.erlide.engine.model.root.IErlProject;
import org.osgi.framework.Bundle;

/**
 * A test project, created from scratch.
 */
@SuppressWarnings("all")
public class ErlideTestProject {
    private final IProject project;

    private final String location;

    private final IErlProject erlProject;

    private ErlideTestProject(final IProject project) {
        this.project = project;
        location = project.getLocation().toOSString();
        final IErlModel model = ErlangEngine.getInstance().getModel();
        final IErlElement _create = model.create(project);
        erlProject = (IErlProject) _create;
        addErlangNature();
        addErlangLibraries();
        createSourceFolder();
        final IFolder binFolder = createBinFolder();
        createOutputFolder(binFolder);
    }

    public ErlideTestProject(final boolean remove, final String projectName) {
        this(new Function0<IProject>() {
            @Override
            public IProject apply() {
                try {
                    IProject _xblockexpression = null;
                    {
                        final IWorkspaceRoot root = ResourcesPlugin.getWorkspace()
                                .getRoot();
                        final IProject project = root.getProject(projectName);
                        if (remove) {
                            project.delete(true, null);
                        }
                        project.create(null);
                        project.open(null);
                        _xblockexpression = project;
                    }
                    return _xblockexpression;
                } catch (final Throwable _e) {
                    throw Exceptions.sneakyThrow(_e);
                }
            }
        }.apply());
    }

    public IFile createFile(final String name, final byte[] content) {
        try {
            IFile _xblockexpression = null;
            {
                final IFile file = project.getFile(name);
                final ByteArrayInputStream inputStream = new ByteArrayInputStream(
                        content);
                file.create(inputStream, true, null);
                _xblockexpression = file;
            }
            return _xblockexpression;
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }

    public IFolder createFolder(final String name) {
        try {
            IFolder _xblockexpression = null;
            {
                final IFolder folder = project.getFolder(name);
                folder.create(true, true, null);
                final IFile keep = project.getFile(name + "/keep");
                final ByteArrayInputStream _byteArrayInputStream = new ByteArrayInputStream(
                        new Function0<byte[]>() {
                            @Override
                            public byte[] apply() {
                                return null;
                            }
                        }.apply());
                keep.create(_byteArrayInputStream, true, null);
                _xblockexpression = folder;
            }
            return _xblockexpression;
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }

    public void dispose() {
        try {
            final boolean _exists = project.exists();
            if (_exists) {
                project.delete(true, true, null);
            } else {
                final File _file = new File(location);
                ErlideTestUtils.deleteRecursive(_file);
            }
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }

    public IFolder createBinFolder() {
        try {
            IFolder _xblockexpression = null;
            {
                final IFolder binFolder = project.getFolder("ebin");
                binFolder.create(false, true, null);
                _xblockexpression = binFolder;
            }
            return _xblockexpression;
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }

    public void addErlangNature() {
        addNature("org.erlide.core.erlnature");
    }

    public void addNature(final String natureId) {
        try {
            final IProjectDescription description = project.getDescription();
            final List<String> ids = IterableExtensions
                    .<String> toList((Iterable<String>) Conversions
                            .doWrapArray(description.getNatureIds()));
            ids.add(natureId);
            description
                    .setNatureIds((String[]) Conversions.unwrapArray(ids, String.class));
            project.setDescription(description, null);
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }

    public void createOutputFolder(final IFolder binFolder) {
        final IPath outputLocation = binFolder.getFullPath();
        final ErlangProjectProperties _properties = erlProject.getProperties();
        _properties.setOutputDir(outputLocation);
    }

    public IErlFolder createSourceFolder() {
        try {
            IErlFolder _xblockexpression = null;
            {
                final IFolder folder = project.getFolder("src");
                folder.create(false, true, null);
                final IErlModel model = ErlangEngine.getInstance().getModel();
                final IErlElement _create = model.create(folder);
                final IErlFolder root = (IErlFolder) _create;
                erlProject.getProperties().getSourceDirs()
                        .add(folder.getProjectRelativePath());
                _xblockexpression = root;
            }
            return _xblockexpression;
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }

    public IErlFolder createIncludeFolder() {
        try {
            IErlFolder _xblockexpression = null;
            {
                final IFolder folder = project.getFolder("include");
                folder.create(false, true, null);
                final IErlModel model = ErlangEngine.getInstance().getModel();
                final IErlElement _create = model.create(folder);
                final IErlFolder root = (IErlFolder) _create;
                erlProject.getProperties().getIncludeDirs()
                        .add(folder.getProjectRelativePath());
                _xblockexpression = root;
            }
            return _xblockexpression;
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }

    public void addErlangLibraries() {
        addToCodepath(null);
    }

    public IPath findFileInPlugin(final String plugin, final String file) {
        Path _xblockexpression = null;
        {
            final Bundle bundle = Platform.getBundle(plugin);
            final URL resource = bundle.getResource(file);
            final String _path = resource.getPath();
            _xblockexpression = new Path(_path);
        }
        return _xblockexpression;
    }

    public String getFileContent(final String filepath) {
        try {
            String _xblockexpression = null;
            {
                final IFile file = project.getFile(filepath);
                final InputStream stream = file.getContents();
                _xblockexpression = ErlideTestUtils.readAndClose(stream);
            }
            return _xblockexpression;
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }

    public void addToCodepath(final IErlFolder folder) {
    }
}
