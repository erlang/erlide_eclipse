package org.erlide.testing.utils

import java.io.ByteArrayInputStream
import java.io.File
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IFolder
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.Path
import org.eclipse.core.runtime.Platform
import org.erlide.engine.ErlangEngine
import org.erlide.engine.model.root.IErlFolder
import org.erlide.engine.model.root.IErlProject

/** A test project, created from scratch.
 *
 */
class ErlideTestProject {

    val IProject project
    val String location
    val IErlProject erlProject

    private new(IProject project) {
        this.project = project
        location = project.getLocation.toOSString

        val model = ErlangEngine.instance.model
        erlProject = model.create(project) as IErlProject
        addErlangNature
        addErlangLibraries
        createSourceFolder
        val binFolder = createBinFolder
        createOutputFolder(binFolder)
    }

    new(boolean remove, String projectName) {
        this({
            val root = ResourcesPlugin.getWorkspace.getRoot
            val project = root.getProject(projectName)
            if (remove)
                project.delete(true, null)
            project.create(null)
            project.open(null)
            project
        })
    }

    def IFile createFile(String name, byte[] content) {
        val file = project.getFile(name)
        val inputStream = new ByteArrayInputStream(content)
        file.create(inputStream, true, null)
        file
    }

    def IFolder createFolder(String name) {
        val folder = project.getFolder(name)
        folder.create(true, true, null)
        val keep = project.getFile(name + "/keep")
        keep.create(new ByteArrayInputStream({
        }), true, null)
        folder
    }

    def void dispose() {
        if (project.exists)
            project.delete(true, true, null)
        else
            ErlideTestUtils.deleteRecursive(new File(location))
    }

    def IFolder createBinFolder() {
        val binFolder = project.getFolder("ebin")
        binFolder.create(false, true, null)
        binFolder
    }

    def void addErlangNature() {
        // can't depend on core plugin
        addNature("org.erlide.core.erlnature")
    }

    def void addNature(String natureId) {
        val description = project.getDescription
        val ids = description.getNatureIds.toList
        ids.add(natureId)
        description.setNatureIds(ids)
        project.setDescription(description, null)
    }

    def void createOutputFolder(IFolder binFolder) {
        val outputLocation = binFolder.getFullPath
        erlProject.properties.outputDir = outputLocation
    }

    def IErlFolder createSourceFolder() {
        val folder = project.getFolder("src")
        folder.create(false, true, null)
        val model = ErlangEngine.instance.model
        val root = model.create(folder) as IErlFolder
        erlProject.properties.sourceDirs.add(folder.projectRelativePath)
        root
    }

    def IErlFolder createIncludeFolder() {
        val folder = project.getFolder("include")
        folder.create(false, true, null)
        val model = ErlangEngine.instance.model
        val root = model.create(folder) as IErlFolder
        erlProject.properties.includeDirs.add(folder.projectRelativePath)
        root
    }

    def void addErlangLibraries() {
        addToCodepath(null)
    }

    def IPath findFileInPlugin(String plugin, String file) {
        val bundle = Platform.getBundle(plugin)
        val resource = bundle.getResource(file)
        new Path(resource.getPath)
    }

    def String getFileContent(String filepath) {
        val file = project.getFile(filepath)
        val stream = file.getContents
        ErlideTestUtils.readAndClose(stream)
    }

    def void addToCodepath(IErlFolder folder) {
        // val cp = erlProject.getRawClasspath
        // erlProject.setRawClasspath(entry +: 1, null)
    }

}
