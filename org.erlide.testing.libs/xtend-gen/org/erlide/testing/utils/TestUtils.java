package org.erlide.testing.utils;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.InputStream;
import java.util.List;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.ListExtensions;
import org.eclipse.xtext.xbase.lib.Pair;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlFolder;
import org.erlide.engine.model.root.IErlProject;

@SuppressWarnings("all")
public class TestUtils {
  public IPath sourceWorkspaceLoc(final String bundleName) {
    throw new Error("Unresolved compilation problems:"
      + "\nThe method or field Platform is undefined for the type TestUtils"
      + "\nThe method or field OSGiUtils is undefined for the type TestUtils"
      + "\ngetBundle cannot be resolved"
      + "\npathInBundle cannot be resolved"
      + "\nget cannot be resolved");
  }
  
  /**
   * Return the Java problem markers corresponding to the given compilation unit.
   */
  public Object findProblemMarkers(final /* ICompilationUnit */Object unit) {
    throw new Error("Unresolved compilation problems:"
      + "\nThe method or field IJavaModelMarker is undefined for the type TestUtils"
      + "\ngetUnderlyingResource cannot be resolved"
      + "\nfindMarkers cannot be resolved"
      + "\nJAVA_MODEL_PROBLEM_MARKER cannot be resolved");
  }
  
  /**
   * Setup the project in the target workspace. The 'name' project should
   *  exist in the source workspace.
   */
  public IErlProject setupProject(final String name, final String bundleName) {
    throw new Error("Unresolved compilation problems:"
      + "\nno viable alternative at input \'val\'"
      + "\nThe method or field EclipseUtils is undefined for the type TestUtils"
      + "\nThe method or field workspace is undefined for the type TestUtils"
      + "\nThe method or field monitor is undefined for the type TestUtils"
      + "\nThe method or field workspace is undefined for the type TestUtils"
      + "\nThe method or field logger is undefined for the type TestUtils"
      + "\nThe method copyDirectory is undefined for the type TestUtils"
      + "\nThe method or field workspace is undefined for the type TestUtils"
      + "\nThe method or field JavaCore is undefined for the type TestUtils"
      + "\nThe method or field ScalaPlugin is undefined for the type TestUtils"
      + "\nThe method or field workspace is undefined for the type TestUtils"
      + "\nworkspaceRunnableIn cannot be resolved"
      + "\n=> cannot be resolved"
      + "\ngetRoot cannot be resolved"
      + "\ngetLocation cannot be resolved"
      + "\ntoFile cannot be resolved"
      + "\ngetAbsolutePath cannot be resolved"
      + "\n+ cannot be resolved"
      + "\n+ cannot be resolved"
      + "\ndebug cannot be resolved"
      + "\ngetRoot cannot be resolved"
      + "\ngetProject cannot be resolved"
      + "\ncreate cannot be resolved"
      + "\nopen cannot be resolved"
      + "\ncreate cannot be resolved"
      + "\ngetScalaProject cannot be resolved"
      + "\ngetRoot cannot be resolved"
      + "\ngetProject cannot be resolved");
  }
  
  public void deleteRecursive(final File d) {
    throw new Error("Unresolved compilation problems:"
      + "\nmissing \';\' at \')\'"
      + "\nThe method Option is undefined for the type TestUtils"
      + "\nThe method or field files is undefined for the type TestUtils"
      + "\nThe method or field file is undefined for the type TestUtils"
      + "\nThe method or field files is undefined for the type TestUtils"
      + "\nThe method or field file is undefined for the type TestUtils"
      + "\nThe method or field file is undefined for the type TestUtils"
      + "\nThe method or field file is undefined for the type TestUtils"
      + "\n< cannot be resolved"
      + "\n- cannot be resolved"
      + "\n< cannot be resolved"
      + "\n- cannot be resolved"
      + "\nisDirectory cannot be resolved"
      + "\ndelete cannot be resolved");
  }
  
  public File createTempDir(final String name) {
    File _xblockexpression = null;
    {
      String _property = System.getProperty("user.home");
      File _file = new File(_property);
      final String userHome = _file.getAbsolutePath();
      final File rootDir = new File(userHome, "ErlCoreTestTempDir");
      final File result = new File(rootDir, name);
      boolean _exists = result.exists();
      if (_exists) {
        this.deleteRecursive(result);
      }
      _xblockexpression = result;
    }
    return _xblockexpression;
  }
  
  public void deleteTempDirs() {
    String _property = System.getProperty("user.home");
    File _file = new File(_property);
    final String userHome = _file.getAbsolutePath();
    final File rootDir = new File(userHome, "SDTCoreTestTempDir");
    boolean _exists = rootDir.exists();
    if (_exists) {
      this.deleteRecursive(rootDir);
    }
  }
  
  /**
   * Add a new file to the given project. The given path is relative to the
   *  project.
   * 
   *  The file must not exist.
   */
  public IFile addFileToProject(final IProject project, final String path, final String content) {
    try {
      String _defaultCharset = project.getDefaultCharset();
      byte[] _bytes = content.getBytes(_defaultCharset);
      return this.addFileToProject(project, path, _bytes);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public IFile addFileToProject(final IProject project, final String path, final byte[] content) {
    throw new Error("Unresolved compilation problems:"
      + "\nThe method init is undefined for the type TestUtils"
      + "\nThe method getFolder is undefined for the type TestUtils"
      + "\nThere is no context to infer the closure\'s argument types from. Consider typing the arguments or put the closures into a typed context."
      + "\nfoldLeft cannot be resolved"
      + "\nexists cannot be resolved"
      + "\n! cannot be resolved"
      + "\ncreate cannot be resolved");
  }
  
  public IFile changeContentOfFile(final IFile file, final String newContent, final String encoding) {
    try {
      IFile _xblockexpression = null;
      {
        byte[] _bytes = newContent.getBytes(encoding);
        ByteArrayInputStream _byteArrayInputStream = new ByteArrayInputStream(_bytes);
        file.setContents(_byteArrayInputStream, 0, null);
        _xblockexpression = file;
      }
      return _xblockexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public List<IMarker> getProblemMarkers(final /* ICompilationUnit */Object... units) {
    throw new Error("Unresolved compilation problems:"
      + "\nflatMap cannot be resolved"
      + "\ntoList cannot be resolved");
  }
  
  public Iterable<Pair<Integer, String>> getErrorMessages(final IProject project) {
    throw new Error("Unresolved compilation problems:"
      + "\nThe method or field IJavaModelMarker is undefined for the type TestUtils"
      + "\nThe method yield is undefined for the type TestUtils"
      + "\nThe method asInstanceOf is undefined for the type TestUtils"
      + "\nThe method or field Int is undefined for the type TestUtils"
      + "\nType mismatch: cannot convert from void to Iterable<Pair<Integer, String>>"
      + "\nJAVA_MODEL_PROBLEM_MARKER cannot be resolved");
  }
  
  public List<String> getErrorMessages(final /* ICompilationUnit */Object... units) {
    throw new Error("Unresolved compilation problems:"
      + "\nThe method or field yieldp is undefined for the type TestUtils"
      + "\nType mismatch: cannot convert from void to List<String>"
      + "\ngetAttribute cannot be resolved"
      + "\ntoString cannot be resolved");
  }
  
  public List<String> buildWith(final IResource resource, final String contents, final Iterable<IErlModule> unitsToWatch) {
    throw new Error("Unresolved compilation problems:"
      + "\nThe method asInstanceOf is undefined for the type TestUtils"
      + "\nThe method or field logger is undefined for the type TestUtils"
      + "\nThe method workspace is undefined for the type TestUtils"
      + "\nType mismatch: cannot convert implicit first argument from TestUtils to IFile"
      + "\nType mismatch: cannot convert from void to List<String>"
      + "\ndebug cannot be resolved"
      + "\nbuild cannot be resolved");
  }
  
  public IProject createProjectInLocalFileSystem(final IFile parentFile, final String projectName) {
    throw new Error("Unresolved compilation problems:"
      + "\nType mismatch: cannot convert from IFile to String"
      + "\nType mismatch: cannot convert from IFile to String");
  }
  
  public String slurpAndClose(final InputStream inputStream) {
    throw new Error("Unresolved compilation problems:"
      + "\nThe method toChar is undefined for the type TestUtils");
  }
  
  public IErlFolder createSourcePackage(final String name, final IErlProject project) {
    throw new Error("Unresolved compilation problems:"
      + "\nThe method javaProject is undefined for the type TestUtils"
      + "\nThe method underlying is undefined for the type TestUtils"
      + "\ngetPackageFragmentRoot cannot be resolved"
      + "\ngetFolder cannot be resolved"
      + "\ncreatePackageFragment cannot be resolved");
  }
  
  public Object createCompilationUnit(final /* IPackageFragment */Object pack, final String name, final String sourceCode) {
    throw new Error("Unresolved compilation problems:"
      + "\ncreateCompilationUnit cannot be resolved");
  }
  
  public void addToClasspath(final IErlProject prj, final /* IClasspathEntry */Object... entries) {
    throw new Error("Unresolved compilation problems:"
      + "\nThe method javaProject is undefined for the type TestUtils"
      + "\nThe method javaProject is undefined for the type TestUtils"
      + "\ngetRawClasspath cannot be resolved"
      + "\nsetRawClasspath cannot be resolved"
      + "\n+ cannot be resolved");
  }
  
  public List<IErlProject> createProjects(final String... names) {
    final Function1<String, IErlProject> _function = new Function1<String, IErlProject>() {
      public IErlProject apply(final String n) {
        return TestUtils.this.createProjectInWorkspace(n, true);
      }
    };
    return ListExtensions.<String, IErlProject>map(((List<String>)Conversions.doWrapArray(names)), _function);
  }
  
  private /* List<Object> */Object internalCreateProjects(final String... names) {
    throw new Error("Unresolved compilation problems:"
      + "\nThe method internalCreateProjectInWorkspace is undefined for the type TestUtils");
  }
  
  public void deleteProjects(final IErlProject... projects) {
    throw new Error("Unresolved compilation problems:"
      + "\nThe method or field EclipseUtils is undefined for the type TestUtils"
      + "\nThe method or field EclipseUtils is undefined for the type TestUtils"
      + "\nThe method foreach is undefined for the type TestUtils"
      + "\nThe method underlying is undefined for the type TestUtils"
      + "\nworkspaceRunnableIn cannot be resolved"
      + "\nworkspaceRoot cannot be resolved"
      + "\ngetWorkspace cannot be resolved"
      + "\ndelete cannot be resolved");
  }
  
  /**
   * Wait until `pred` is true, or timeout (in ms).
   */
  public void waitUntil(final int timeout, final Function0<? extends Boolean> pred) {
    try {
      final long start = System.currentTimeMillis();
      Boolean cond = pred.apply();
      while (((System.currentTimeMillis() < (start + timeout)) && (!(cond).booleanValue()))) {
        {
          Thread.sleep(100);
          Boolean _apply = pred.apply();
          cond = _apply;
        }
      }
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  /**
   * Create a project in the current workspace. If `withSourceRoot` is true,
   *  it creates a source folder called `src`.
   */
  public IErlProject createProjectInWorkspace(final String projectName, final boolean withSourceRoot) {
    throw new Error("Unresolved compilation problems:"
      + "\nno viable alternative at input \',\'"
      + "\nIClassPathEntry cannot be resolved to a type."
      + "\nThe method or field ErlangCore is undefined for the type TestUtils"
      + "\nThe method or field model is undefined for the type TestUtils"
      + "\nThe method or field JavaRuntime is undefined for the type TestUtils"
      + "\nThe method or field JavaCore is undefined for the type TestUtils"
      + "\nThe method or field JavaCore is undefined for the type TestUtils"
      + "\nThe method or field SdtConstants is undefined for the type TestUtils"
      + "\nThe method or field IClasspathEntry is undefined for the type TestUtils"
      + "\nThe method or field ScalaPlugin is undefined for the type TestUtils"
      + "\nNATURE_ID cannot be resolved"
      + "\ncreate cannot be resolved"
      + "\nsetOutputLocation cannot be resolved"
      + "\n+= cannot be resolved"
      + "\ngetDefaultJREContainerEntry cannot be resolved"
      + "\ngetPackageFragmentRoot cannot be resolved"
      + "\n+= cannot be resolved"
      + "\nnewSourceEntry cannot be resolved"
      + "\ngetPath cannot be resolved"
      + "\n+= cannot be resolved"
      + "\nnewContainerEntry cannot be resolved"
      + "\nScalaLibContId cannot be resolved"
      + "\nsetRawClasspath cannot be resolved"
      + "\ntoArray cannot be resolved"
      + "\n< cannot be resolved"
      + "\n> cannot be resolved"
      + "\ngetScalaProject cannot be resolved");
  }
  
  public <T extends Object> T withWorkspacePreference(final String name, final boolean value, final Function0<? extends T> thunk) {
    throw new Error("Unresolved compilation problems:"
      + "\nThe method getPreferenceStore is undefined for the type TestUtils"
      + "\nType mismatch: cannot convert from ()=>T to T"
      + "\nType mismatch: cannot convert from ()=>T to T"
      + "\ngetBoolean cannot be resolved"
      + "\nsetValue cannot be resolved"
      + "\nsetValue cannot be resolved");
  }
  
  public void buildWorkspace() {
    try {
      IWorkspace _workspace = ResourcesPlugin.getWorkspace();
      NullProgressMonitor _nullProgressMonitor = new NullProgressMonitor();
      _workspace.build(IncrementalProjectBuilder.INCREMENTAL_BUILD, _nullProgressMonitor);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}
