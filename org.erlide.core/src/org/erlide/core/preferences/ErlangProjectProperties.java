package org.erlide.core.preferences;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.erlide.jinterface.backend.RuntimeVersion;
import org.erlide.jinterface.backend.util.PreferencesUtils;
import org.osgi.service.prefs.BackingStoreException;
import org.osgi.service.prefs.Preferences;

/**
 * Project properties.
 * 
 */
public class ErlangProjectProperties {

	private RuntimeVersion requiredRuntimeVersion;

	private List<SourceLocation> sources = new ArrayList<SourceLocation>();
	private List<String> includes = new ArrayList<String>();
	private String output;
	private boolean allowOutputPerSource = false;
	private final List<DependencyLocation> dependencies = new ArrayList<DependencyLocation>();
	private final List<CodePathLocation> codePathOrder = new ArrayList<CodePathLocation>();
	private final Map<String, String> macros = new HashMap<String, String>();

	public ErlangProjectProperties() {
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

	public String setOutput(String output) {
		return this.output = output;
	}

	public Collection<DependencyLocation> getDependencies() {
		return Collections.unmodifiableCollection(dependencies);
	}

	public void addDependencies(Collection<DependencyLocation> locations) {
		for (DependencyLocation loc : locations) {
			if (!dependencies.contains(loc)) {
				dependencies.add(loc);
			}
		}
	}

	public void removeDependencies(Collection<DependencyLocation> locations) {
		for (DependencyLocation loc : locations) {
			dependencies.remove(loc);
		}
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

	public Collection<ProjectLocation> getProjectDependencies() {
		return Collections.unmodifiableCollection(filter(dependencies,
				ProjectLocation.class));
	}

	public Collection<LibraryLocation> getLibraryDependencies() {
		return Collections.unmodifiableCollection(filter(dependencies,
				LibraryLocation.class));
	}

	public Collection<CodePathLocation> getCodePathOrder() {
		return Collections.unmodifiableCollection(codePathOrder);
	}

	public RuntimeVersion getRequiredRuntimeVersion() {
		return requiredRuntimeVersion;
	}

	public Map<String, String> getMacros() {
		return macros;
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

	public void setAllowOutputPerSource(boolean allow) {
		allowOutputPerSource = allow;
	}

	public boolean doesAllowOutputPerSource() {
		return allowOutputPerSource;
	}

	public void setRequiredRuntimeVersion(RuntimeVersion runtimeVersion) {
		requiredRuntimeVersion = runtimeVersion;
	}

	public void addSources(Collection<SourceLocation> mkSources) {
		// TODO Auto-generated method stub

	}

	public void addIncludes(Collection<String> includesList) {
		// TODO Auto-generated method stub

	}

}
