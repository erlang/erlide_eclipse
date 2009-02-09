package org.erlide.core;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Assert;

public class NewErlangProjectProperties {

	public static class DependencyLocation {
	}

	public static final class SourceLocation extends DependencyLocation {
		private String directory;
		private List<String> includePatterns = new ArrayList<String>();
		private List<String> excludePatterns = new ArrayList<String>();
		private String output;
		private Map<String, String> compilerOptions = new HashMap<String, String>();
		private Map<String, Map<String, String>> fileCompilerOptions = new HashMap<String, Map<String, String>>();

		public SourceLocation(String directory, List<String> includePatterns,
				List<String> excludePatterns, String output,
				Map<String, String> compilerOptions,
				Map<String, Map<String, String>> fileCompilerOptions) {
			Assert.isLegal(directory != null,
					"SourceLocation requires a non-null directory");
			this.directory = directory;
			if (includePatterns != null) {
				this.includePatterns = includePatterns;
			}
			if (excludePatterns != null) {
				this.excludePatterns = excludePatterns;
			}
			this.output = output;
			if (compilerOptions != null) {
				this.compilerOptions = compilerOptions;
			}
			if (fileCompilerOptions != null) {
				this.fileCompilerOptions = fileCompilerOptions;
			}
		}

		public String getDirectory() {
			return directory;
		}

		public List<String> getIncludePatterns() {
			return includePatterns;
		}

		public List<String> getExcludePatterns() {
			return excludePatterns;
		}

		public String getOutput() {
			return output;
		}

		public Map<String, String> getCompilerOptions() {
			return compilerOptions;
		}

		public Map<String, Map<String, String>> getFileCompilerOptions() {
			return fileCompilerOptions;
		}
	}

	public static final class LibraryLocation extends DependencyLocation {
		private List<SourceLocation> sources = new ArrayList<SourceLocation>();
		private List<String> includes = new ArrayList<String>();
		private String output;
		private List<LibraryLocation> libraries = new ArrayList<LibraryLocation>();

		public LibraryLocation(List<SourceLocation> sources,
				List<String> includes, String output,
				List<LibraryLocation> libraries) {
			if (sources != null) {
				this.sources = sources;
			}
			if (includes != null) {
				this.includes = includes;
			}
			this.output = output;
			if (libraries != null) {
				this.libraries = libraries;
			}
		}

		public List<SourceLocation> getSources() {
			return sources;
		}

		public List<String> getIncludes() {
			return includes;
		}

		public String getOutput() {
			return output;
		}

		public List<LibraryLocation> getLibraries() {
			return libraries;
		}
	}

	public static final class ProjectLocation extends DependencyLocation {
		private IProject project;

		public ProjectLocation(IProject project) {
			Assert.isLegal(project != null,
					"ProjectLocation requires a non-null project");
			this.project = project;
		}

		public IProject getProject() {
			return project;
		}
	}

	private List<SourceLocation> sources = new ArrayList<SourceLocation>();
	private List<String> includes = new ArrayList<String>();
	private String output;
	private Map<String, String> compilerOptions = new HashMap<String, String>();
	private List<ProjectLocation> projects = new ArrayList<ProjectLocation>();
	private List<LibraryLocation> libraries = new ArrayList<LibraryLocation>();
	private List<WeakReference<DependencyLocation>> codePathOrder = new ArrayList<WeakReference<DependencyLocation>>();

	public NewErlangProjectProperties() {
		output = "ebin";
	}

	public NewErlangProjectProperties(ErlangProjectProperties oldProperties) {
		output = "ebin";
	}

	public List<SourceLocation> getSources() {
		return sources;
	}

	public List<String> getIncludes() {
		return includes;
	}

	public String getOutput() {
		return output;
	}

	public Map<String, String> getCompilerOptions() {
		return compilerOptions;
	}

	public List<ProjectLocation> getProjects() {
		return projects;
	}

	public List<LibraryLocation> getLibraries() {
		return libraries;
	}

	public List<WeakReference<DependencyLocation>> getCodePathOrder() {
		return codePathOrder;
	}
}
