package org.erlide.runtime.runtimeinfo

import com.google.common.collect.Lists
import java.io.File
import java.io.IOException
import java.util.Collection
import java.util.List
import java.util.Set
import org.erlide.util.SystemConfiguration

class RuntimeFinder {

	val static locations = #["c:/program files", "c:/program files (x86)", "c:/programs", "c:/", "c:/apps", "/usr",
		"/usr/lib", "/usr/lib64", "/usr/local", "/usr/local/lib", "/Library/Frameworks/erlang/Versions"]

	def static Collection<RuntimeInfo> guessRuntimeLocations() {
		val List<RuntimeInfo> result = newArrayList()
		val homeDir = SystemConfiguration.instance.homeDir
		val syspath = System.getenv("PATH").split(File.pathSeparator)
		val Set<String> locs = newHashSet(syspath + locations)
		locs.add(homeDir)
		val envRuntime = System.getProperty("erlide.runtime")
		if(envRuntime !== null) locs.add(envRuntime)
		locs.addAll(getKerlLocations())

		for (String loc : locs) {
			val roots = findRuntime(loc)
			for (File root : roots) {
				val RuntimeInfo rt = new RuntimeInfo.Builder().withName(root.name).withHomeDir(root.path).build()
				result.add(rt)
			}
		}
		println("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
		println(result)
		return result
	}

	def static Iterable<String> getKerlLocations() {
		val List<String> result = Lists.newArrayList()
		val ProcessBuilder builder = new ProcessBuilder(#["kerl", "list", "installations"])
		try {
			val Process process = builder.start()
			val StringBuilder line = new StringBuilder()
			var int chr
			while ((chr = process.inputStream.read()) !== -1) {
				if (chr === 10 || chr === 13) {
					if (line.length != 0) {
						result.add(line.toString())
						line.length = 0
					}
				} else {
					line.append(chr as char)
				}
			}
			if (line.length != 0) {
				result.add(line.toString())
			}
		} catch (IOException e) {
			// ignore, kerl is not available or usable
			// ErlLogger.warn(e)
		}
		return result.map[if(split(" ", 2).length > 2) split(" ", 2).get(1) else null].filterNull
	}

	def private static Collection<File> findRuntime(String loc) {
		val Collection<File> result = newArrayList()
		if (loc === null) {
			return result
		}
		val File folder = new File(loc)
		if (!folder.exists()) {
			return result
		}
		if (RuntimeInfo.validateLocation(folder.path)) {
			result.add(folder)
			return result
		}
		val files = folder.listFiles
		if (files !== null) {
			for (File f : files) {
				if (f.isDirectory && RuntimeInfo.validateLocation(f.path)) {
					result.add(f)
				}
			}
		}
		return result
	}

}
