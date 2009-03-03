package org.erlide.core;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.erlide.runtime.PreferencesUtils;
import org.erlide.runtime.ProjectPreferencesConstants;
import org.erlide.runtime.backend.RuntimeInfo;
import org.erlide.runtime.backend.RuntimeVersion;
import org.osgi.service.prefs.BackingStoreException;
import org.osgi.service.prefs.Preferences;

public class NewErlangProjectProperties {

	private List<SourceLocation> sources = new ArrayList<SourceLocation>();
	private List<String> includes = new ArrayList<String>();
	private String output;
	private Map<String, String> compilerOptions = new HashMap<String, String>();
	private List<DependencyLocation> dependencies = new ArrayList<DependencyLocation>();
	private List<WeakReference<CodePathLocation>> codePathOrder = new ArrayList<WeakReference<CodePathLocation>>();
	private RuntimeVersion requiredRuntimeVersion;

	public NewErlangProjectProperties() {
		output = "ebin";
		// TODO fixme
	}

	public NewErlangProjectProperties(ErlangProjectProperties old) {
		requiredRuntimeVersion = old.getRuntimeVersion();
		if (!requiredRuntimeVersion.isDefined()) {
			RuntimeInfo runtimeInfo = old.getRuntimeInfo();
			if (runtimeInfo != null) {
				requiredRuntimeVersion = runtimeInfo.getVersion();
			}
		}

		sources = mkSources(old.getSourceDirs());
		includes = PreferencesUtils.unpackList(old.getIncludeDirsString());
		output = old.getOutputDir();
		compilerOptions.put("debug_info", "true");

		IPathVariableManager pvman = ResourcesPlugin.getWorkspace()
				.getPathVariableManager();

		String exmodf = old.getExternalModulesFile();
		IPath ff = pvman.resolvePath(new Path(exmodf));
		List<String> externalModules = PreferencesUtils.readFile(ff.toString());
		List<SourceLocation> sloc = makeSourceLocations(externalModules);

		String exincf = old.getExternalModulesFile();
		ff = pvman.resolvePath(new Path(exincf));
		// List<String> exinc = PreferencesUtils.readFile(ff.toString());
		List<String> externalIncludes = null;// PreferencesUtils.unpackList(exinc);

		LibraryLocation loc = new LibraryLocation(sloc, externalIncludes, null,
				null);
		dependencies.add(loc);
	}

	private List<SourceLocation> makeSourceLocations(
			List<String> externalModules) {
		List<SourceLocation> result = new ArrayList<SourceLocation>();

		List<String> modules = new ArrayList<String>();
		for (String mod : externalModules) {
			if (mod.endsWith(".erlidex")) {
				List<String> mods = PreferencesUtils.readFile(mod);
				modules.addAll(mods);
			} else {
				modules.add(mod);
			}
		}

		Map<String, List<String>> grouped = new HashMap<String, List<String>>();
		for (String mod : modules) {
			int i = mod.lastIndexOf('/');
			String path = mod.substring(0, i);
			String file = mod.substring(i + 1);

			System.out.println("FOUND: '" + path + "' '" + file + "'");
			List<String> pval = grouped.get(path);
			if (pval == null) {
				pval = new ArrayList<String>();
			}
			pval.add(file);
			grouped.put(path.toString(), pval);
		}
		System.out.println(grouped);

		for (Entry<String, List<String>> loc : grouped.entrySet()) {
			SourceLocation location = new SourceLocation(loc.getKey(), loc
					.getValue(), null, null, null, null);
			result.add(location);
		}

		return result;
	}

	private List<SourceLocation> mkSources(String[] sourceDirs) {
		List<SourceLocation> result = new ArrayList<SourceLocation>();
		for (String src : sourceDirs) {
			result.add(new SourceLocation(src, null, null, null, null, null));
		}
		return result;
	}

	public Collection<SourceLocation> getSources() {
		return Collections.unmodifiableCollection(sources);
	}

	public Collection<String> getIncludes() {
		return Collections.unmodifiableCollection(includes);
	}

	public String getOutput() {
		return output;
	}

	public Map<String, String> getCompilerOptions() {
		return Collections.unmodifiableMap(compilerOptions);
	}

	public Collection<DependencyLocation> getDependencies() {
		return Collections.unmodifiableCollection(dependencies);
	}

	public Collection<ProjectLocation> getProjects() {
		return Collections.unmodifiableCollection(filter(dependencies,
				ProjectLocation.class));
	}

	@SuppressWarnings("unchecked")
	private static <U, T extends U> Collection<T> filter(
			Collection<U> dependencies2, Class<T> class1) {
		List<T> result = new ArrayList<T>();
		for (U oo : dependencies2) {
			if (oo.getClass().equals(class1)) {
				result.add((T) oo);
			}
		}
		return result;
	}

	public Collection<LibraryLocation> getLibraries() {
		return Collections.unmodifiableCollection(filter(dependencies,
				LibraryLocation.class));
	}

	public Collection<WeakReference<CodePathLocation>> getCodePathOrder() {
		return Collections.unmodifiableCollection(codePathOrder);
	}

	public RuntimeVersion getRequiredRuntimeVersion() {
		return requiredRuntimeVersion;
	}

	public void load(IEclipsePreferences root) throws BackingStoreException {
		output = root.get(ProjectPreferencesConstants.OUTPUT, "ebin");
		requiredRuntimeVersion = new RuntimeVersion(root.get(
				ProjectPreferencesConstants.REQUIRED_BACKEND_VERSION, null));
		includes = PreferencesUtils.unpackList(root.get(
				ProjectPreferencesConstants.INCLUDES, ""));
		Preferences srcNode = root.node(ProjectPreferencesConstants.SOURCES);
		sources.clear();
		for (String src : srcNode.childrenNames()) {
			IEclipsePreferences sn = (IEclipsePreferences) srcNode.node(src);
			SourceLocation loc = new SourceLocation(sn);
			sources.add(loc);
		}
	}

	public void store(IEclipsePreferences root) throws BackingStoreException {
		PreferencesUtils.clearAll(root);
		root.put(ProjectPreferencesConstants.OUTPUT, output);
		if (requiredRuntimeVersion != null) {
			root.put(ProjectPreferencesConstants.REQUIRED_BACKEND_VERSION,
					requiredRuntimeVersion.toString());
		}
		root.put(ProjectPreferencesConstants.INCLUDES, PreferencesUtils
				.packList(includes));
		Preferences srcNode = root.node(ProjectPreferencesConstants.SOURCES);
		for (SourceLocation loc : sources) {
			loc.store((IEclipsePreferences) srcNode.node(Integer.toString(loc
					.getId())));
		}

		root.flush();
	}

}
