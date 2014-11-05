package org.erlide.testing.utils;

import java.util.List;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.ListExtensions;
import org.erlide.engine.internal.model.root.ErlProject;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlFolder;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.testing.utils.ProjectBuilder;

@SuppressWarnings("all")
public class TestProjectSetup extends ProjectBuilder {
  private String projectName;
  
  private String srcRoot = "/%s/src/";
  
  private String bundleName = "org.scala-ide.sdt.core.tests";
  
  private ErlProject internalProject;
  
  /**
   * The project corresponding to projectName, after copying to the test
   * workspace.
   */
  private IErlProject project;
  
  /**
   * The package root corresponding to /src inside the project.
   */
  private IErlFolder srcPackageRoot;
  
  public TestProjectSetup(final String projectName, final String srcRoot, final String bundleName) {
    throw new Error("Unresolved compilation problems:"
      + "\nThe method internalSetupProject is undefined for the type TestProjectSetup"
      + "\nThe method assertNotNull is undefined for the type TestProjectSetup");
  }
  
  public IFile file(final String path) {
    IProject _workspaceProject = this.project.getWorkspaceProject();
    return _workspaceProject.getFile(path);
  }
  
  /**
   * Return the compilation unit corresponding to the given path, relative to the src folder.
   *  for example: "scala/collection/Map.scala"
   */
  public IErlModule compilationUnit(final String path) {
    try {
      return this.srcPackageRoot.findModule(path, null);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  /**
   * Return a sequence of compilation units corresponding to the given paths.
   */
  public Iterable<IErlModule> compilationUnits(final String... paths) {
    final Function1<String, IErlModule> _function = new Function1<String, IErlModule>() {
      public IErlModule apply(final String it) {
        return TestProjectSetup.this.compilationUnit(it);
      }
    };
    return ListExtensions.<String, IErlModule>map(((List<String>)Conversions.doWrapArray(paths)), _function);
  }
  
  public IErlModule createSourceFile(final String packageName, final String unitName, final String contents) {
    throw new Error("Unresolved compilation problems:"
      + "\nThe method createSourcePackage is undefined for the type TestProjectSetup"
      + "\nThe method createCompilationUnit is undefined for the type TestProjectSetup");
  }
  
  public /* IMarker */Object findMarker(final String marker) {
    throw new Error("Unresolved compilation problems:"
      + "\nInvalid number of arguments. The method findMarker(String) is not applicable for the arguments (Class<TestUtils>,String)"
      + "\nType mismatch: cannot convert from Class<TestUtils> to String");
  }
  
  /**
   * Emulate the opening of a scala source file (i.e., it tries to
   * reproduce the steps performed by JDT when opening a file in an editor).
   * 
   * @param srcPath the path to the scala source file
   */
  public abstract Object open(final /* srcPath */Object __unknown__);
}
