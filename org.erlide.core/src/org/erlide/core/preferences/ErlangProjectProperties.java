package org.erlide.core.preferences;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.erlide.jinterface.backend.RuntimeVersion;
import org.osgi.service.prefs.BackingStoreException;

import com.google.common.collect.Sets;

/**
 * Project properties.
 *
 */
public final class ErlangProjectProperties {

    private RuntimeVersion requiredRuntimeVersion;

    private final Set<SourceEntry> sources = Sets.newHashSet();
    private Set<IPath> includes = Sets.newHashSet();
    private IPath output;
    private final Set<PathEntry> dependencies = Sets.newHashSet();
    private final Map<String, String> macros = new HashMap<String, String>();

    public ErlangProjectProperties() {
    }

    public Collection<SourceEntry> getSources() {
        return Collections.unmodifiableCollection(sources);
    }

    public Collection<IPath> getIncludes() {
        return Collections.unmodifiableCollection(includes);
    }

    public IPath getOutput() {
        return output;
    }

    public void setOutput(IPath output) {
        this.output = output;
    }

    public Collection<PathEntry> getDependencies() {
        return Collections.unmodifiableCollection(dependencies);
    }

    public void addDependencies(Collection<PathEntry> locations) {
        for (PathEntry loc : locations) {
            if (!dependencies.contains(loc)) {
                dependencies.add(loc);
            }
        }
    }

    public void removeDependencies(Collection<PathEntry> locations) {
        for (PathEntry loc : locations) {
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

    public Collection<ProjectEntry> getProjectDependencies() {
        return Collections.unmodifiableCollection(filter(dependencies,
                ProjectEntry.class));
    }

    public Collection<LibraryEntry> getLibraryDependencies() {
        return Collections.unmodifiableCollection(filter(dependencies,
                LibraryEntry.class));
    }

    public RuntimeVersion getRequiredRuntimeVersion() {
        return requiredRuntimeVersion;
    }

    public Map<String, String> getMacros() {
        return macros;
    }

    public void load(final IEclipsePreferences root)
            throws BackingStoreException {
        // TODO implement!
    }

    public void store(final IEclipsePreferences root)
            throws BackingStoreException {
        // TODO implement!
    }

    public void setRequiredRuntimeVersion(RuntimeVersion runtimeVersion) {
        requiredRuntimeVersion = runtimeVersion;
    }

    public void addSources(Collection<SourceEntry> mkSources) {
        // TODO Auto-generated method stub

    }

    public void addIncludes(Collection<String> includesList) {
        // TODO Auto-generated method stub

    }
}
