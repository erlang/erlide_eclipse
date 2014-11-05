package org.erlide.testing.utils

import org.eclipse.core.resources.IFile
import org.erlide.engine.internal.model.root.ErlProject
import org.erlide.engine.model.erlang.IErlModule
import org.erlide.engine.model.root.IErlFolder
import org.erlide.engine.model.root.IErlProject

import static extension java.lang.String.*

class TestProjectSetup extends ProjectBuilder {

    String projectName
    String srcRoot = "/%s/src/"
    String bundleName = "org.scala-ide.sdt.core.tests"

    ErlProject internalProject

    /**
     * The project corresponding to projectName, after copying to the test
     * workspace.
     */
    IErlProject project

    /** The package root corresponding to /src inside the project. */
    IErlFolder srcPackageRoot

    new(String projectName, String srcRoot, String bundleName) {
        this.projectName = projectName
        this.srcRoot = srcRoot
        this.bundleName = bundleName
        internalProject = TestUtils.internalSetupProject(projectName, bundleName)
        project = internalProject

        srcPackageRoot = project.getChildNamed(srcRoot.format(projectName)) as IErlFolder
        assertNotNull(srcPackageRoot)
        srcPackageRoot.open(null)
    }

    def IFile file(String path) {
        return project.getWorkspaceProject().getFile(path)
    }

    /** Return the compilation unit corresponding to the given path, relative to the src folder.
       *  for example: "scala/collection/Map.scala"
       */
    def IErlModule compilationUnit(String path) {
        srcPackageRoot.findModule(path, null)
    }

    /** Return a sequence of compilation units corresponding to the given paths. */
    def Iterable<IErlModule> compilationUnits(String... paths) { paths.map[compilationUnit] }

    def IErlModule createSourceFile(String packageName, String unitName, String contents) {
        val pack = TestUtils.createSourcePackage(packageName, project)
        TestUtils.createCompilationUnit(pack, unitName, contents)
    }

    def IMarker findMarker(String marker) { TestUtils.findMarker(marker) }

    /** Emulate the opening of a scala source file (i.e., it tries to
       * reproduce the steps performed by JDT when opening a file in an editor).
       *
       * @param srcPath the path to the scala source file
       * */
    def open(srcPath :String)
: ScalaSourceFile = {
        val unit = scalaCompilationUnit(srcPath)
        openWorkingCopyFor(unit)
        reload(unit)
        unit
      }

/** Open a working copy of the passed `unit` */
private def openWorkingCopyFor( unit: ScalaSourceFile) {
        val requestor = mock(classOf[IProblemRequestor])
        // the requestor must be active, or unit.getWorkingCopy won't trigger the Scala
        // structure builder
        when(requestor.isActive()).thenReturn(true)

        val owner = new WorkingCopyOwner() {
          override def getProblemRequestor(unit: org.eclipse.jdt.core.ICompilationUnit): IProblemRequestor = requestor
        }

        // this will trigger the Scala structure builder
        unit.getWorkingCopy(owner, new NullProgressMonitor)
      }

      /** Wait until the passed `unit` is entirely typechecked. */
      def waitUntilTypechecked( unit: ScalaCompilationUnit) {
        // give a chance to the background compiler to report the error
        unit.withSourceFile { (source, compiler) =>
          compiler.askLoadedTyped(source, true).get // wait until unit is typechecked
        }
      }

      /** Open the passed `source` and wait until it has been fully typechecked.*/
      def openAndWaitUntilTypechecked( source: ScalaSourceFile) {
        val sourcePath = source.getPath()
        val projectSrcPath = project.underlying.getFullPath() append "src"
        val path = sourcePath.makeRelativeTo(projectSrcPath)
        open(path.toOSString())
        waitUntilTypechecked(source)
      }

}
