/**
 * Copyright (c) 2014 1C LLC. All rights reserved. This program and the accompanying
 * materials are made available under the terms of the Eclipse Public License v1.0 which
 * accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors: Vladimir Piskarev (1C) - initial API and implementation Vlad Dumitrescu -
 * updated to Junit 4
 */
package org.erlide.testing.utils;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.collect.Lists;
import com.google.common.io.Files;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import org.eclipse.core.filesystem.URIUtil;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceDescription;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.jobs.IJobManager;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.IErlangEngine;
import org.erlide.engine.ModelActivator;
import org.erlide.engine.internal.old_model.erlang.ErlAttribute;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.ErlangProjectProperties;
import org.erlide.engine.model.builder.BuilderProperties;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.old_model.IErlModel;
import org.erlide.engine.old_model.erlang.IErlModule;
import org.erlide.engine.old_model.root.IErlElement;
import org.erlide.engine.old_model.root.IErlElementLocator;
import org.erlide.engine.old_model.root.IErlProject;
import org.erlide.util.ErlLogger;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;

/**
 * A workspace test case.
 * <p>
 * Provides a number of useful methods, such as {@link #setUpProject(String)}.
 * </p>
 * <p>
 * Also, in its {@link #setUp()} and {@link #tearDown()} methods, this class enforces some
 * common rules for workspace tests:
 * <ul>
 * <li>each test suite starts running in a clean workspace with auto-build turned off</li>
 * <li>each test is responsible for setting up the necessary workspace state</li>
 * <li>after running each test suite, the workspace is cleaned up.</li>
 * </ul>
 * </p>
 */
@SuppressWarnings("all")
public class WorkspaceTest {
  /**
   * Turns auto-build off, cleans up the workspace, inits the model.
   */
  @Before
  public void setUp() throws Exception {
    this.tearDown();
    ModelActivator.initModel();
    this.setAutoBuilding(false);
    IWorkspaceRoot _workspaceRoot = this.getWorkspaceRoot();
    _workspaceRoot.setDefaultCharset("UTF-8", null);
  }
  
  /**
   * Cleans up the workspace.
   */
  @After
  public void tearDown() throws Exception {
    this.cleanUpWorkspace();
    IErlModel _oldModel = this.getOldModel();
    _oldModel.close();
    ModelActivator.cleanupStateDir();
  }
  
  /**
   * Shortcut to <code>ResourcesPlugin.workspace</code>
   */
  protected IWorkspace getWorkspace() {
    return ResourcesPlugin.getWorkspace();
  }
  
  /**
   * Shortcut to <code>workspace.root</code>
   */
  protected IWorkspaceRoot getWorkspaceRoot() {
    IWorkspace _workspace = this.getWorkspace();
    return _workspace.getRoot();
  }
  
  protected IErlModel getOldModel() {
    IErlangEngine _instance = ErlangEngine.getInstance();
    return _instance.getOldModel();
  }
  
  /**
   * Shortcut to <code>getWorkspaceRoot().getProject(name)</code>
   * 
   * @param name
   *            the name of the project
   * @return the project (never <code>null</code>)
   */
  protected IProject getProject(final String name) {
    IWorkspaceRoot _workspaceRoot = this.getWorkspaceRoot();
    return _workspaceRoot.getProject(name);
  }
  
  /**
   * Creates a new project in the workspace by copying its content from the OSGi-bundle
   * of this test case. The content must reside in the folder <code>/workspace/</code>
   * &ltproject-name&gt inside the bundle.
   * 
   * @param name
   *            the name of the project
   * @return the created and opened project (never <code>null</code>)
   * @throws CoreException
   * @throws IOException
   */
  protected IProject setUpProject(final String name) throws CoreException, IOException {
    URL _workspaceURL = this.getWorkspaceURL();
    this.setUpFile(_workspaceURL, name);
    final IProject project = this.getProject(name);
    final IWorkspaceRunnable runnable = new IWorkspaceRunnable() {
      @Override
      public void run(final IProgressMonitor monitor) throws CoreException {
        project.create(monitor);
        project.open(monitor);
      }
    };
    IWorkspace _workspace = this.getWorkspace();
    _workspace.run(runnable, null);
    return project;
  }
  
  protected void setUpFile(final URL resourceURL, final String path) throws IOException {
    Assert.assertNotNull(resourceURL);
    final URL fileURL = FileLocator.toFileURL(resourceURL);
    File sourceRoot = null;
    try {
      URI _uRI = fileURL.toURI();
      File _file = new File(_uRI);
      sourceRoot = _file;
    } catch (final Throwable _t) {
      if (_t instanceof URISyntaxException) {
        final URISyntaxException e = (URISyntaxException)_t;
        throw new IOException(e);
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
    Assert.assertNotNull(sourceRoot);
    final File source = new File(sourceRoot, path);
    String _plus = (source + " doesn\'t exist");
    boolean _exists = source.exists();
    Assert.assertTrue(_plus, _exists);
    IWorkspaceRoot _workspaceRoot = this.getWorkspaceRoot();
    IPath _location = _workspaceRoot.getLocation();
    final File targetRoot = _location.toFile();
    final File target = new File(targetRoot, path);
    this.copy(source, target);
  }
  
  protected void setUpTmpFile(final String path) throws IOException {
    URL _workspaceURL = this.getWorkspaceURL();
    this.setUpFile(_workspaceURL, ("tmp/" + path));
  }
  
  public URL getWorkspaceURL() {
    Class<? extends WorkspaceTest> _class = this.getClass();
    ClassLoader _classLoader = _class.getClassLoader();
    return _classLoader.getResource("workspace");
  }
  
  protected IErlProject getErlProject(final String name) {
    IErlModel _oldModel = this.getOldModel();
    IWorkspaceRoot _workspaceRoot = this.getWorkspaceRoot();
    IProject _project = _workspaceRoot.getProject(name);
    return _oldModel.getErlangProject(_project);
  }
  
  protected IErlModule getErlModule(final String name) {
    try {
      IErlangEngine _instance = ErlangEngine.getInstance();
      final IErlElementLocator locator = _instance.getElementLocator();
      return locator.findModule(name);
    } catch (final Throwable _t) {
      if (_t instanceof ErlModelException) {
        final ErlModelException e = (ErlModelException)_t;
        return null;
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
  }
  
  /**
   * Sets the property "Build Automatically" for the workspace.
   * 
   * @param value
   *            boolean
   * @throws CoreException
   */
  protected void setAutoBuilding(final boolean value) throws CoreException {
    IWorkspace _workspace = this.getWorkspace();
    final IWorkspaceDescription description = _workspace.getDescription();
    boolean _isAutoBuilding = description.isAutoBuilding();
    boolean _notEquals = (value != _isAutoBuilding);
    if (_notEquals) {
      description.setAutoBuilding(value);
      IWorkspace _workspace_1 = this.getWorkspace();
      _workspace_1.setDescription(description);
    }
  }
  
  /**
   * Builds the workspace, waiting for build completion.
   * 
   * @throws CoreException
   */
  protected void buildWorkspace() throws CoreException {
    IWorkspace _workspace = this.getWorkspace();
    _workspace.build(IncrementalProjectBuilder.INCREMENTAL_BUILD, null);
    this.waitForBuildCompletion();
  }
  
  /**
   * Waits for build completion.
   */
  protected void waitForBuildCompletion() {
    boolean wasInterrupted = false;
    do {
      try {
        IJobManager _jobManager = Job.getJobManager();
        _jobManager.join(ResourcesPlugin.FAMILY_AUTO_BUILD, null);
        wasInterrupted = false;
      } catch (final Throwable _t) {
        if (_t instanceof OperationCanceledException) {
          final OperationCanceledException e = (OperationCanceledException)_t;
          e.printStackTrace();
        } else if (_t instanceof InterruptedException) {
          final InterruptedException e_1 = (InterruptedException)_t;
          wasInterrupted = true;
        } else {
          throw Exceptions.sneakyThrow(_t);
        }
      }
    } while(wasInterrupted);
  }
  
  /**
   * Deletes all resources in the workspace.
   * 
   * @throws CoreException
   */
  protected void cleanUpWorkspace() {
    try {
      IWorkspaceRoot _workspaceRoot = this.getWorkspaceRoot();
      _workspaceRoot.delete((IResource.ALWAYS_DELETE_PROJECT_CONTENT & IResource.FORCE), null);
    } catch (final Throwable _t) {
      if (_t instanceof Exception) {
        final Exception e = (Exception)_t;
        ErlLogger.error(e);
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
    IWorkspaceRoot _workspaceRoot_1 = this.getWorkspaceRoot();
    IPath _location = _workspaceRoot_1.getLocation();
    String _portableString = _location.toPortableString();
    final File file = new File(_portableString, "tmp");
    this.deleteDirectory(file);
  }
  
  private boolean deleteDirectory(final File directory) {
    boolean _exists = directory.exists();
    if (_exists) {
      final File[] files = directory.listFiles();
      if ((null != files)) {
        for (final File f : files) {
          boolean _isDirectory = f.isDirectory();
          if (_isDirectory) {
            this.deleteDirectory(f);
          } else {
            f.delete();
          }
        }
      }
    }
    return directory.delete();
  }
  
  /**
   * Copy the given source (a file or a directory) to the given destination directory.
   */
  protected void copy(final File source, final File dest) throws IOException {
    boolean _exists = source.exists();
    boolean _not = (!_exists);
    if (_not) {
      return;
    }
    boolean _isDirectory = source.isDirectory();
    if (_isDirectory) {
      this.createDir(dest);
      final String[] files = source.list();
      if ((files != null)) {
        for (final String file : files) {
          {
            final File sourceFile = new File(source, file);
            boolean _isDirectory_1 = sourceFile.isDirectory();
            if (_isDirectory_1) {
              final File destSubDir = new File(dest, file);
              this.copy(sourceFile, destSubDir);
            } else {
              this.copy(sourceFile, dest);
            }
          }
        }
      }
    } else {
      File destDir = null;
      boolean _isDirectory_1 = dest.isDirectory();
      if (_isDirectory_1) {
        destDir = dest;
      } else {
        File _parentFile = dest.getParentFile();
        destDir = _parentFile;
      }
      this.createDir(destDir);
      String _name = source.getName();
      final File destFile = new File(destDir, _name);
      boolean _createNewFile = destFile.createNewFile();
      boolean _not_1 = (!_createNewFile);
      if (_not_1) {
        String _plus = (destFile + " already exists");
        throw new IOException(_plus);
      }
      Files.copy(source, destFile);
    }
  }
  
  private void createDir(final File dest) throws IOException {
    boolean _exists = dest.exists();
    boolean _not = (!_exists);
    if (_not) {
      boolean _mkdirs = dest.mkdirs();
      boolean _not_1 = (!_mkdirs);
      if (_not_1) {
        throw new IOException(("Could not create directory " + dest));
      }
    }
  }
  
  private void buildPaths(final IProject project, final Collection<IPath> paths) throws CoreException {
    for (final IPath path : paths) {
      boolean _pathIsOk = this.pathIsOk(path);
      if (_pathIsOk) {
        final IFolder folder = project.getFolder(path);
        this.createFolder(folder);
      }
    }
  }
  
  private boolean pathIsOk(final IPath path) {
    boolean _and = false;
    boolean _and_1 = false;
    boolean _isAbsolute = path.isAbsolute();
    boolean _not = (!_isAbsolute);
    if (!_not) {
      _and_1 = false;
    } else {
      String _string = path.toString();
      boolean _equals = _string.equals(".");
      boolean _not_1 = (!_equals);
      _and_1 = _not_1;
    }
    if (!_and_1) {
      _and = false;
    } else {
      boolean _isEmpty = path.isEmpty();
      boolean _not_2 = (!_isEmpty);
      _and = _not_2;
    }
    return _and;
  }
  
  protected void createFolder(final IFolder folder) throws CoreException {
    boolean _exists = folder.exists();
    boolean _not = (!_exists);
    if (_not) {
      final IContainer parent = folder.getParent();
      if ((parent instanceof IFolder)) {
        this.createFolder(((IFolder)parent));
      }
      folder.create(false, true, null);
    }
  }
  
  public IErlModule createModule(final IErlProject project, final String moduleName, final String moduleContents) throws CoreException {
    IProject _workspaceProject = project.getWorkspaceProject();
    final IFolder folder = _workspaceProject.getFolder("src");
    final IErlModule module = this.createModule(folder, moduleName, moduleContents);
    return module;
  }
  
  public IErlModule createInclude(final IErlProject project, final String moduleName, final String moduleContents) throws CoreException {
    IProject _workspaceProject = project.getWorkspaceProject();
    final IFolder folder = _workspaceProject.getFolder("include");
    final IErlModule module = this.createModule(folder, moduleName, moduleContents);
    return module;
  }
  
  private IErlModule createModule(final IFolder folder, final String moduleName, final String moduleContents) throws CoreException {
    final IFile file = this.createFile(folder, moduleName, moduleContents);
    final IErlModel model = this.getOldModel();
    IErlangEngine _instance = ErlangEngine.getInstance();
    final IErlElementLocator locator = _instance.getElementLocator();
    IErlModule module = locator.findModule(file);
    if ((module != null)) {
      module.close();
    }
    if ((module == null)) {
      IPath _location = file.getLocation();
      final String path = _location.toPortableString();
      String _name = file.getName();
      Charset _defaultCharset = Charset.defaultCharset();
      String _name_1 = _defaultCharset.name();
      IErlModule _moduleFromFile = model.getModuleFromFile(model, _name, path, _name_1, path);
      module = _moduleFromFile;
    }
    return module;
  }
  
  public IFile createFile(final IFolder folder, final String name, final String contents) throws CoreException {
    final IFile file = folder.getFile(name);
    IPath _location = file.getLocation();
    String _oSString = _location.toOSString();
    final File f = new File(_oSString);
    f.delete();
    Charset _defaultCharset = Charset.defaultCharset();
    byte[] _bytes = contents.getBytes(_defaultCharset);
    ByteArrayInputStream _byteArrayInputStream = new ByteArrayInputStream(_bytes);
    file.create(_byteArrayInputStream, true, null);
    return file;
  }
  
  public void deleteModule(final IErlModule module) throws CoreException {
    module.dispose();
    final String scannerName = module.getScannerName();
    IResource _resource = module.getResource();
    final IFile file = ((IFile) _resource);
    boolean _and = false;
    if (!(file != null)) {
      _and = false;
    } else {
      boolean _exists = file.exists();
      _and = _exists;
    }
    if (_and) {
      file.delete(true, null);
    }
    IErlangEngine _instance = ErlangEngine.getInstance();
    IPath _stateDir = _instance.getStateDir();
    String _portableString = _stateDir.toPortableString();
    final IPath stateDir = new Path(_portableString);
    final List<String> cacheExts = Collections.<String>unmodifiableList(CollectionLiterals.<String>newArrayList(".noparse", ".refs", ".scan"));
    for (final String ext : cacheExts) {
      {
        final IPath p = stateDir.append((scannerName + ext));
        String _oSString = p.toOSString();
        final File f = new File(_oSString);
        boolean _exists_1 = f.exists();
        if (_exists_1) {
          f.delete();
        }
      }
    }
  }
  
  public IErlProject createProject(final String name, final IPath path) throws CoreException {
    final IProject project2 = this.getProject(name);
    boolean _exists = project2.exists();
    if (_exists) {
      project2.delete(true, null);
    }
    IErlModel _oldModel = this.getOldModel();
    String _portableString = path.toPortableString();
    final IErlProject erlProject = _oldModel.newProject(name, _portableString);
    BuilderProperties _builderProperties = erlProject.getBuilderProperties();
    _builderProperties.setBuilderTool(BuilderTool.INTERNAL);
    final IProject project = erlProject.getWorkspaceProject();
    final ErlangProjectProperties prefs = erlProject.getProperties();
    final ArrayList<IPath> srcDirs = new ArrayList<IPath>();
    Path _path = new Path("src");
    srcDirs.add(_path);
    prefs.setSourceDirs(srcDirs);
    this.buildPaths(project, srcDirs);
    final ArrayList<IPath> includeDirs = new ArrayList<IPath>();
    Path _path_1 = new Path("include");
    includeDirs.add(_path_1);
    this.buildPaths(project, includeDirs);
    prefs.setIncludeDirs(includeDirs);
    final Path ebinDir = new Path("ebin");
    ArrayList<IPath> _newArrayList = Lists.<IPath>newArrayList(ebinDir);
    this.buildPaths(project, _newArrayList);
    prefs.setOutputDir(ebinDir);
    erlProject.open(null);
    return erlProject;
  }
  
  public IErlProject getExistingProject(final String name) {
    IWorkspaceRoot _workspaceRoot = this.getWorkspaceRoot();
    final IProject project = _workspaceRoot.getProject(name);
    IErlModel _oldModel = this.getOldModel();
    return _oldModel.getErlangProject(project);
  }
  
  public IPath getTmpPath(final String fileName) {
    final String tmpdir = System.getProperty("java.io.tmpdir");
    Path _path = new Path(tmpdir);
    return _path.append(fileName);
  }
  
  public URI getTmpURIPath(final String fileName) {
    IPath _tmpPath = this.getTmpPath(fileName);
    String _portableString = _tmpPath.toPortableString();
    return URIUtil.toURI(_portableString);
  }
  
  public File createTmpFile(final String fileName, final String contentString) throws IOException, FileNotFoundException {
    IPath _tmpPath = this.getTmpPath(fileName);
    final String pathString = _tmpPath.toOSString();
    final File f = new File(pathString);
    boolean _exists = f.exists();
    if (_exists) {
      f.delete();
    }
    f.createNewFile();
    final FileOutputStream fileOutputStream = new FileOutputStream(pathString);
    byte[] _bytes = contentString.getBytes();
    fileOutputStream.write(_bytes);
    fileOutputStream.close();
    return f;
  }
  
  public void deleteProject(final IErlProject erlProject) throws CoreException {
    final IProject project = erlProject.getWorkspaceProject();
    final IPath location = project.getLocation();
    try {
      project.delete(true, null);
    } catch (final Throwable _t) {
      if (_t instanceof Exception) {
        final Exception e = (Exception)_t;
        ErlLogger.error(e);
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
    if ((location != null)) {
      String _portableString = location.toPortableString();
      File _file = new File(_portableString);
      _file.delete();
    }
    erlProject.dispose();
    IErlModel _oldModel = this.getOldModel();
    _oldModel.resourceChanged(null);
    IErlModel _oldModel_1 = this.getOldModel();
    _oldModel_1.open(null);
  }
  
  public IErlModule createModuleFromText(final String name, final String initialText) {
    IErlModel _oldModel = this.getOldModel();
    IErlModel _oldModel_1 = this.getOldModel();
    final IErlModule module = _oldModel.getModuleFromText(_oldModel_1, name, initialText, name);
    return module;
  }
  
  public IErlProject createTmpErlProject(final String projectName) throws CoreException {
    IPath _tmpPath = this.getTmpPath(projectName);
    return this.createProject(projectName, _tmpPath);
  }
  
  public IErlElement createErlAttribute(final IErlElement parent, final String name, final OtpErlangObject value, final String extra, final int sourceRangeOffset, final int sourceRangeLength) {
    final ErlAttribute attribute = new ErlAttribute(parent, name, value, extra);
    attribute.setSourceRangeOffset(sourceRangeOffset);
    attribute.setSourceRangeLength(sourceRangeLength);
    return attribute;
  }
}
