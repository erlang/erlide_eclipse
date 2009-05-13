package org.erlide.core.preferences;

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
import org.erlide.jinterface.backend.RuntimeInfo;
import org.erlide.jinterface.backend.RuntimeVersion;
import org.erlide.jinterface.backend.util.PreferencesUtils;
import org.osgi.service.prefs.BackingStoreException;
import org.osgi.service.prefs.Preferences;

public class NewErlangProjectProperties {

	private List<SourceLocation> sources = new ArrayList<SourceLocation>();
	private List<String> includes = new ArrayList<String>();
	private String output;
	private final Map<String, String> compilerOptions = new HashMap<String, String>();
	private final List<DependencyLocation> dependencies = new ArrayList<DependencyLocation>();
	private final List<WeakReference<CodePathLocation>> codePathOrder = new ArrayList<WeakReference<CodePathLocation>>();
	private RuntimeVersion requiredRuntimeVersion;

	public NewErlangProjectProperties() {
		output = "ebin";
		// TODO fixme
	}

	public NewErlangProjectProperties(final ErlangProjectProperties old) {
		requiredRuntimeVersion = old.getRuntimeVersion();
		if (!requiredRuntimeVersion.isDefined()) {
			final RuntimeInfo runtimeInfo = old.getRuntimeInfo();
			if (runtimeInfo != null) {
				requiredRuntimeVersion = runtimeInfo.getVersion();
			}
		}

		sources = mkSources(old.getSourceDirs());
		includes = PreferencesUtils.unpackList(old.getIncludeDirsString());
		output = old.getOutputDir();
		compilerOptions.put("debug_info", "true");

		final IPathVariableManager pvman = ResourcesPlugin.getWorkspace()
				.getPathVariableManager();

		final String exmodf = old.getExternalModulesFile();
		IPath ff = pvman.resolvePath(new Path(exmodf));
		final List<String> externalModules = PreferencesUtils.readFile(ff
				.toString());
		final List<SourceLocation> sloc = makeSourceLocations(externalModules);

		final String exincf = old.getExternalModulesFile();
		ff = pvman.resolvePath(new Path(exincf));
		// List<String> exinc = PreferencesUtils.readFile(ff.toString());
		final List<String> externalIncludes = null;// PreferencesUtils.unpackList(exinc);

		final LibraryLocation loc = new LibraryLocation(sloc, externalIncludes,
				null, null);
		dependencies.add(loc);
	}

	private List<SourceLocation> makeSourceLocations(
			final List<String> externalModules) {
		final List<SourceLocation> result = new ArrayList<SourceLocation>();

		final List<String> modules = new ArrayList<String>();
		for (final String mod : externalModules) {
			if (mod.endsWith(".erlidex")) {
				final List<String> mods = PreferencesUtils.readFile(mod);
				modules.addAll(mods);
			} else {
				modules.add(mod);
			}
		}

		final Map<String, List<String>> grouped = new HashMap<String, List<String>>();
		for (final String mod : modules) {
			final int i = mod.lastIndexOf('/');
			final String path = mod.substring(0, i);
			final String file = mod.substring(i + 1);

			System.out.println("FOUND: '" + path + "' '" + file + "'");
			List<String> pval = grouped.get(path);
			if (pval == null) {
				pval = new ArrayList<String>();
			}
			pval.add(file);
			grouped.put(path.toString(), pval);
		}
		System.out.println(grouped);

		for (final Entry<String, List<String>> loc : grouped.entrySet()) {
			final SourceLocation location = new SourceLocation(loc.getKey(),
					loc.getValue(), null, null, null, null);
			result.add(location);
		}

		return result;
	}

	private List<SourceLocation> mkSources(final String[] sourceDirs) {
		final List<SourceLocation> result = new ArrayList<SourceLocation>();
		for (final String src : sourceDirs) {
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
			final Collection<U> dependencies2, final Class<T> class1) {
		final List<T> result = new ArrayList<T>();
		for (final U oo : dependencies2) {
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

	public void load(final IEclipsePreferences root)
			throws BackingStoreException {
		output = root.get(ProjectPreferencesConstants.OUTPUT, "ebin");
		requiredRuntimeVersion = new RuntimeVersion(root.get(
				ProjectPreferencesConstants.REQUIRED_BACKEND_VERSION, null));
		includes = PreferencesUtils.unpackList(root.get(
				ProjectPreferencesConstants.INCLUDES, ""));
		final Preferences srcNode = root
				.node(ProjectPreferencesConstants.SOURCES);
		sources.clear();
		for (final String src : srcNode.childrenNames()) {
			final IEclipsePreferences sn = (IEclipsePreferences) srcNode
					.node(src);
			final SourceLocation loc = new SourceLocation(sn);
			sources.add(loc);
		}
	}

	public void store(final IEclipsePreferences root)
			throws BackingStoreException {
		CodePathLocation.clearAll(root);
		root.put(ProjectPreferencesConstants.OUTPUT, output);
		if (requiredRuntimeVersion != null) {
			root.put(ProjectPreferencesConstants.REQUIRED_BACKEND_VERSION,
					requiredRuntimeVersion.toString());
		}
		root.put(ProjectPreferencesConstants.INCLUDES, PreferencesUtils
				.packList(includes));
		final Preferences srcNode = root
				.node(ProjectPreferencesConstants.SOURCES);
		for (final SourceLocation loc : sources) {
			loc.store((IEclipsePreferences) srcNode.node(Integer.toString(loc
					.getId())));
		}

		root.flush();
	}

}
