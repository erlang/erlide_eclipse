package org.erlide.testing.utils;

import com.google.common.collect.Iterables;
import java.io.ByteArrayInputStream;
import java.util.Collections;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.IErlangEngine;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlFolder;
import org.erlide.engine.model.root.IErlProject;

/**
 * A test project, created from scratch.
 */
@SuppressWarnings("all")
public class TestProject {
  private final IProject project;
  
  private final String location;
  
  private final IErlProject erlProject;
  
  private final IErlFolder sourceFolder;
  
  private final IErlFolder ebinFolder;
  
  public TestProject(final IProject project) {
    throw new Error("Unresolved compilation problems:"
      + "\nThe method or field addOtpLibraries is undefined for the type TestProject"
      + "\nThe method or field createEbinFolder is undefined for the type TestProject"
      + "\nType mismatch: cannot convert from IErlFolder to IFolder");
  }
  
  public TestProject() {
    this(false);
  }
  
  public TestProject(final boolean remove) {
    this(remove, "project-1");
  }
  
  public TestProject(final boolean remove, final String projectName) {
    this(
      new Function0<IProject>() {
        public IProject apply() {
          try {
            IProject _xblockexpression = null;
            {
              IWorkspace _workspace = ResourcesPlugin.getWorkspace();
              final IWorkspaceRoot root = _workspace.getRoot();
              final IProject project = root.getProject(projectName);
              if (remove) {
                project.delete(true, null);
              }
              project.create(null);
              project.open(null);
              _xblockexpression = project;
            }
            return _xblockexpression;
          } catch (Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
          }
        }
      }.apply());
  }
  
  protected IErlModel getModel() {
    IErlangEngine _instance = ErlangEngine.getInstance();
    return _instance.getModel();
  }
  
  public IErlModule createModule(final IErlFolder pack, final String cuName, final String source) {
    throw new Error("Unresolved compilation problems:"
      + "\nThe method getElementName is undefined for the type TestProject"
      + "\nThe method createCompilationUnit is undefined for the type TestProject"
      + "\nType mismatch: cannot convert from int to IErlModule"
      + "\ngetTypes cannot be resolved");
  }
  
  public IFile createFile(final String name, final byte[] content) {
    try {
      IFile _xblockexpression = null;
      {
        final IFile file = this.project.getFile(name);
        final ByteArrayInputStream inputStream = new ByteArrayInputStream(content);
        file.create(inputStream, true, null);
        _xblockexpression = file;
      }
      return _xblockexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public IFolder createFolder(final String name) {
    try {
      IFolder _xblockexpression = null;
      {
        final IFolder folder = this.project.getFolder(name);
        folder.create(true, true, null);
        final IFile keep = this.project.getFile((name + "/keep"));
        ByteArrayInputStream _byteArrayInputStream = new ByteArrayInputStream(new byte[] {});
        keep.create(_byteArrayInputStream, true, null);
        _xblockexpression = folder;
      }
      return _xblockexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public Object dispose() {
    throw new Error("Unresolved compilation problems:"
      + "\nThe method deleteRecursive is undefined for the type TestProject");
  }
  
  public IFolder createBinFolder() {
    try {
      IFolder _xblockexpression = null;
      {
        final IFolder binFolder = this.project.getFolder("bin");
        binFolder.create(false, true, null);
        _xblockexpression = binFolder;
      }
      return _xblockexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public void addErlangNature() {
    throw new Error("Unresolved compilation problems:"
      + "\nThe method or field ErlangCore is undefined for the type TestProject"
      + "\nNATURE_ID cannot be resolved");
  }
  
  public void addNature(final String natureId) {
    try {
      final IProjectDescription description = this.project.getDescription();
      String[] _natureIds = description.getNatureIds();
      Iterable<String> _plus = Iterables.<String>concat(((Iterable<? extends String>)Conversions.doWrapArray(_natureIds)), Collections.<String>unmodifiableList(CollectionLiterals.<String>newArrayList(natureId)));
      description.setNatureIds(((String[])Conversions.unwrapArray(_plus, String.class)));
      this.project.setDescription(description, null);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public Object createOutputFolder(final IFolder binFolder) {
    throw new Error("Unresolved compilation problems:"
      + "\nThe method or field javaProject is undefined for the type TestProject"
      + "\nsetOutputLocation cannot be resolved");
  }
  
  public IErlFolder createSourceFolder() {
    throw new Error("Unresolved compilation problems:"
      + "\nThe method or field javaProject is undefined for the type TestProject"
      + "\nThe method addToClasspath is undefined for the type TestProject"
      + "\nThe method or field JavaCore is undefined for the type TestProject"
      + "\ngetPackageFragmentRoot cannot be resolved"
      + "\nnewSourceEntry cannot be resolved"
      + "\ngetPath cannot be resolved");
  }
  
  public Object addJavaSystemLibraries() {
    throw new Error("Unresolved compilation problems:"
      + "\nThe method addToClasspath is undefined for the type TestProject"
      + "\nThe method or field JavaRuntime is undefined for the type TestProject"
      + "\ngetDefaultJREContainerEntry cannot be resolved");
  }
  
  public Object addScalaSystemLibraries() {
    throw new Error("Unresolved compilation problems:"
      + "\nThe method addToClasspath is undefined for the type TestProject"
      + "\nThe method or field JavaCore is undefined for the type TestProject"
      + "\nThe method or field SdtConstants is undefined for the type TestProject"
      + "\nnewContainerEntry cannot be resolved"
      + "\nScalaLibContId cannot be resolved");
  }
  
  public IPath findFileInPlugin(final String plugin, final String file) {
    throw new Error("Unresolved compilation problems:"
      + "\nThe method or field Platform is undefined for the type TestProject"
      + "\ngetBundle cannot be resolved"
      + "\ngetResource cannot be resolved"
      + "\ngetPath cannot be resolved");
  }
  
  public String getFileContent(final String filepath) {
    throw new Error("Unresolved compilation problems:"
      + "\nThe method slurpAndClose is undefined for the type TestProject");
  }
}
