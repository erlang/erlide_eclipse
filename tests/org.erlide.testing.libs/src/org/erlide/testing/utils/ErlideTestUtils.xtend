package org.erlide.testing.utils

import java.io.File
import java.io.InputStream

//import org.eclipse.core.resources.IWorkspace
//import org.eclipse.core.resources.ResourcesPlugin
//import org.erlide.annotations.Lazy
class ErlideTestUtils {

	// @Lazy
	// var IWorkspace workspace = ResourcesPlugin.getWorkspace
	//
	// def void enableAutoBuild(boolean enable) {
	// // auto-building is on
	// val desc = workspace.getDescription
	// desc.setAutoBuilding(enable)
	// workspace.setDescription(desc)
	// }
	def static void deleteRecursive(File d) {
		if (d.exists) {
			val filesOpt = d.listFiles
			for (file : filesOpt) {
				if (file.isDirectory)
					deleteRecursive(file)
				else
					file.delete
			}
			d.delete
		}
	}

	def File createTempDir(String name) {
		val userHome = new File(System.getProperty("user.home")).getAbsolutePath
		val rootDir = new File(userHome, "ErlangCoreTestTempDir")
		val result = new File(rootDir, name)
		if (result.exists)
			deleteRecursive(result)
		result
	}

	def void deleteTempDirs() {
		val userHome = new File(System.getProperty("user.home")).getAbsolutePath
		val rootDir = new File(userHome, "ErlangCoreTestTempDir")
		if (rootDir.exists)
			deleteRecursive(rootDir)
	}

	def static String readAndClose(InputStream inputStream) {
		val stringBuilder = new StringBuilder
		try {
			var ch = inputStream.read
			while (ch != -1) {
				stringBuilder.append(ch as char)
				ch = inputStream.read
			}
		} finally {
			inputStream.close
		}
		stringBuilder.toString
	}

}
