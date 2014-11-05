package org.erlide.testing.utils;

import java.io.ByteArrayInputStream
import java.io.File
import java.io.InputStream
import java.util.List
import javax.swing.text.html.Option
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IMarker
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.IncrementalProjectBuilder
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.core.runtime.Path
import org.erlide.engine.ModelActivator
import org.erlide.engine.internal.model.root.ErlProject
import org.erlide.engine.model.erlang.IErlModule
import org.erlide.engine.model.root.IErlFolder
import org.erlide.engine.model.root.IErlProject
import org.erlide.util.FileUtils

import static extension java.lang.String.*
import org.erlide.core.ErlangCore
import java.util.regex.Pattern
import java.util.regex.Matcher

public class TestUtils {

    def IPath sourceWorkspaceLoc(String bundleName) {
        val bundle = Platform.getBundle(bundleName)
        OSGiUtils.pathInBundle(bundle, File.separatorChar + "test-workspace").get
    }

    /** Return the Java problem markers corresponding to the given compilation unit. */
    def findProblemMarkers(ICompilationUnit unit) {
        unit.getUnderlyingResource().findMarkers(IJavaModelMarker.JAVA_MODEL_PROBLEM_MARKER, true,
            IResource.DEPTH_INFINITE)
    }

    /** Setup the project in the target workspace. The 'name' project should
       *  exist in the source workspace.
       */
    def IErlProject setupProject(String name, String bundleName) {
        EclipseUtils.workspaceRunnableIn(workspace)
        {
            monitor =>
                val wspaceLoc = workspace.getRoot.getLocation
                val src = new File(sourceWorkspaceLoc(bundleName).toFile().getAbsolutePath + File.separatorChar + name)
                val dst = new File(wspaceLoc.toFile().getAbsolutePath + File.separatorChar + name)
                logger.debug("copying %s to %s".format(src, dst))
                FileUtils.copyDirectory(src, dst)
                val project = workspace.getRoot.getProject(name)
                project.create(null)
                project.open(null)
                JavaCore.create(project)
            }
            ScalaPlugin().getScalaProject(workspace.getRoot.getProject(name))
        }

        def void deleteRecursive(File d) {
            if (d.exists) {
                val filesOpt = Option(d.listFiles)
                for (files < - filesOpt; file < - files)
                    if (file.isDirectory)
                        deleteRecursive(file)
                    else
                        file.delete
                d.delete
            }
        }

        def File createTempDir(String name) {
            val userHome = new File(System.getProperty("user.home")).getAbsolutePath
            val rootDir = new File(userHome, "ErlCoreTestTempDir")
            val result = new File(rootDir, name)
            if (result.exists)
                deleteRecursive(result)
            result
        }

        def void deleteTempDirs() {
            val userHome = new File(System.getProperty("user.home")).getAbsolutePath
            val rootDir = new File(userHome, "SDTCoreTestTempDir")
            if (rootDir.exists)
                deleteRecursive(rootDir)
        }

        /** Add a new file to the given project. The given path is relative to the
       *  project.
       *
       *  The file must not exist.
       */
        def IFile addFileToProject(IProject project, String path, String content) {
            addFileToProject(project, path, content.getBytes(project.getDefaultCharset()))
        }

        def IFile addFileToProject(IProject project, String path, byte[] content) {
            val filePath = new Path(path)
            val dirNames = filePath.segments.init // last segment is the file
            dirNames.foldLeft(project) [ container, segment |
                val folder = container.getFolder(new Path(segment))
                if (!folder.exists())
                    folder.create(false, true, null)
                folder
            ]

            val file = project.getFile(filePath);
            file.create(new ByteArrayInputStream(content), true, null)
            file
        }

        def IFile changeContentOfFile(IFile file, String newContent, String encoding) {
            file.setContents(new ByteArrayInputStream(newContent.getBytes(encoding)), 0, null)
            file
        }

        def List<IMarker> getProblemMarkers(ICompilationUnit... units) {
            units.flatMap(findProblemMarkers).toList
        }

        def Iterable<Pair<Integer, String>> getErrorMessages(IProject project) {
            for (m : project.findMarkers(IJavaModelMarker.JAVA_MODEL_PROBLEM_MARKER, true, IResource.DEPTH_INFINITE))
                yield(m.getAttribute(IMarker.SEVERITY).asInstanceOf[Int], m.getAttribute(IMarker.MESSAGE).toString)
        }

        def List<String> getErrorMessages(ICompilationUnit... units) {
            for (p : getProblemMarkers(units))
                yieldp.getAttribute(IMarker.MESSAGE).toString
        }

        def List<String> buildWith(IResource resource, String contents, Iterable<IErlModule> unitsToWatch) {
            changeContentOfFile(resource.asInstanceOf[IFile], contents)

            logger.debug("=== Rebuilding workspace === ")
            TestUtils.workspace.build(IncrementalProjectBuilder.INCREMENTAL_BUILD, null)

            val problems = getProblemMarkers(unitsToWatch)

            for (p : problems)
                p.getAttribute(IMarker.MESSAGE).toString
        }

        def IProject createProjectInLocalFileSystem(IFile parentFile, String projectName) {
            val project = ResourcesPlugin.getWorkspace.getRoot.getProject(projectName)
            if (project.exists)
                project.delete(true, null)
            val testFile = new File(parentFile, projectName)
            if (testFile.exists)
                deleteRecursive(testFile)

            val desc = ResourcesPlugin.getWorkspace.newProjectDescription(projectName)
            desc.setLocation(new Path(new File(parentFile, projectName).getPath))
            project.create(desc, null)
            project.open(null)
            project
        }

        def String slurpAndClose(InputStream inputStream) {
            val stringBuilder = new StringBuilder
            try {
                var ch = 0
                while ({
                    ch = inputStream.read;
                    ch
                } != -1) {
                    stringBuilder.append(ch.toChar)
                }
            } finally {
                inputStream.close
            }
            stringBuilder.toString
        }

        def IErlFolder createSourcePackage(String name, IErlProject project) {
            project.javaProject.getPackageFragmentRoot(project.underlying.getFolder("/src")).
                createPackageFragment(name, true, null)
        }

        def createCompilationUnit(IPackageFragment pack, String name, String sourceCode) {
            val cu = pack.createCompilationUnit(name, sourceCode, false, null)
            Thread.sleep(200)
            cu
        }

        def void addToClasspath(IErlProject prj, IClasspathEntry... entries) {
            val existing = prj.javaProject.getRawClasspath
            prj.javaProject.setRawClasspath(existing + entries, null)
        }

        def createProjects(String... names) {
            names.map[n|createProjectInWorkspace(n, true)]
        }

        private def internalCreateProjects(String... names) {
            names.map[n|internalCreateProjectInWorkspace(n, true)]
        }

        def void deleteProjects(IErlProject... projects) {
            EclipseUtils.workspaceRunnableIn(EclipseUtils.workspaceRoot.getWorkspace) [
                projects.foreach[it.underlying.delete(true, null)]
            ]
        }

        /** Wait until `pred` is true, or timeout (in ms). */
        def void waitUntil(int timeout, ()=>boolean pred) {
            val start = System.currentTimeMillis()
            var cond = pred.apply
            while ((System.currentTimeMillis() < start + timeout) && !cond) {
                Thread.sleep(100)
                cond = pred.apply
            }
        }

        /** Create a project in the current workspace. If `withSourceRoot` is true,
       *  it creates a source folder called `src`.
       */
        def IErlProject createProjectInWorkspace(String projectName, boolean withSourceRoot) {
            val workspace = ResourcesPlugin.getWorkspace()
            val workspaceRoot = workspace.getRoot()
            val project = workspaceRoot.getProject(projectName)
            project.create(null)
            project.open(null)

            val description = project.getDescription()
            description.setNatureIds(#[ErlangCore.NATURE_ID])
            project.setDescription(description, null)

            val javaProject = model.create(project)
            javaProject.setOutputLocation(new Path("/" + projectName + "/bin"), null)

            val IClassPathEntry[] entries = #[]
            entries += JavaRuntime.getDefaultJREContainerEntry()

            if (withSourceRoot) {
                val sourceFolder = project.getFolder("/src")
                sourceFolder.create(false, true, null)
                val root = javaProject.getPackageFragmentRoot(sourceFolder)
                entries += JavaCore.newSourceEntry(root.getPath())
            }

            entries += JavaCore.newContainerEntry(Path.fromPortableString(SdtConstants.ScalaLibContId))
            javaProject.setRawClasspath(
                entries.toArray < IClasspathEntry >
                    , null)

                ScalaPlugin().getScalaProject(project)
            }

            def <T> T withWorkspacePreference(String name, boolean value, ()=>T thunk) {
                val store = ModelActivator.getPreferenceStore
                val old = store.getBoolean(name)
                try {
                    store.setValue(name, value)
                    thunk
                } finally {
                    store.setValue(name, old)
                }
            }

            def void buildWorkspace() {
                ResourcesPlugin.getWorkspace().build(IncrementalProjectBuilder.INCREMENTAL_BUILD,
                    new NullProgressMonitor())
            }
        }
