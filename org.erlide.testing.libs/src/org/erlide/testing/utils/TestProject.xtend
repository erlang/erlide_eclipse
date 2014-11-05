package org.erlide.testing.utils;

import java.io.ByteArrayInputStream
import java.io.File
import org.eclipse.core.resources.IFolder
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.Path
import org.erlide.core.ErlangCore
import org.erlide.engine.ErlangEngine
import org.erlide.engine.model.IErlModel
import org.erlide.engine.model.erlang.IErlModule
import org.erlide.engine.model.root.IErlFolder
import org.erlide.engine.model.root.IErlProject

/** A test project, created from scratch.
     */
public class TestProject {

    val IProject project
    val String location
    val IErlProject erlProject
    val IErlFolder sourceFolder
    val IErlFolder ebinFolder

    new(IProject project) {
        this.project = project
        location = project.getLocation.toOSString

        erlProject = model.create(project) as IErlProject
        addErlangNature
        addOtpLibraries
        sourceFolder = createSourceFolder
        ebinFolder = createEbinFolder
        createOutputFolder(ebinFolder)
    }

    new() {
        this(false)
    }

    new(boolean remove) {
        this(remove, "project-1")
    }

    new(boolean remove, String projectName) {
        this(
            {

                val root = ResourcesPlugin.getWorkspace.getRoot

                val project = root.getProject(projectName)
                if (remove)
                    project.delete(true, null)
                project.create(null)
                project.open(null)
                project
            })
    }

    protected def IErlModel getModel() {
        ErlangEngine.instance.model
    }

    def IErlModule createModule(IErlFolder pack, String cuName, String source) {
        val buf = new StringBuffer
        buf.append("package " + pack.getElementName() + ";\n")
        buf.append("\n")
        buf.append(source)
        val cu = pack.createCompilationUnit(cuName, buf.toString(), false, null)
        cu.getTypes()
        (0)
    }

    def createFile(String name, byte[] content) {
        val file = project.getFile(name)
        val inputStream = new ByteArrayInputStream(content)
        file.create(inputStream, true, null)
        file
    }

    def createFolder(String name) {
        val folder = project.getFolder(name)
        folder.create(true, true, null)
        val keep = project.getFile(name + "/keep")
        keep.create(new ByteArrayInputStream(#[]), true, null)
        folder
    }

    def dispose() {
        if (project.exists)
            project.delete(true, true, null)
        else
            TestUtils.deleteRecursive(new File(location))
    }

    def createBinFolder() {
        val binFolder = project.getFolder("bin")
        binFolder.create(false, true, null)
        binFolder
    }

    def addErlangNature() {
        addNature(ErlangCore.NATURE_ID)
    }

    def addNature(String natureId) {
        val description = project.getDescription
        description.setNatureIds(description.getNatureIds + #[natureId])

        project.setDescription(description, null)
    }

    def createOutputFolder(IFolder binFolder) {
        val outputLocation = binFolder.getFullPath
        javaProject.setOutputLocation(outputLocation, null)
    }

    def IErlFolder createSourceFolder() {
        val folder = project.getFolder("src")
        folder.create(false, true, null)
        val root = javaProject.getPackageFragmentRoot(folder)
        addToClasspath(JavaCore.newSourceEntry(root.getPath))
        root
    }

    def addJavaSystemLibraries() {
        addToClasspath(JavaRuntime.getDefaultJREContainerEntry)
    }

    def addScalaSystemLibraries() {
        addToClasspath(JavaCore.newContainerEntry(Path.fromPortableString(SdtConstants.ScalaLibContId)))
    }

    def IPath findFileInPlugin(String plugin, String file) {
        val bundle = Platform.getBundle(plugin)
        val resource = bundle.getResource(file)
        new Path(resource.getPath)
    }

    def String getFileContent(String filepath) {
        val file = project.getFile(filepath)
        val stream = file.getContents
        TestUtils.slurpAndClose(stream)
    }

}
