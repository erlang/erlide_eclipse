package org.erlide.core.model.erlang;

import java.util.Collection;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.erlide.core.backend.runtimeinfo.RuntimeInfo;
import org.erlide.core.backend.runtimeinfo.RuntimeVersion;
import org.osgi.service.prefs.BackingStoreException;

public interface IOldErlangProjectProperties {

    void store() throws BackingStoreException;

    Collection<IPath> getIncludeDirs();

    void setIncludeDirs(final Collection<IPath> includeDirs2);

    IPath getOutputDir();

    void setOutputDir(final IPath dir);

    Collection<IPath> getSourceDirs();

    void setSourceDirs(final Collection<IPath> sourceDirs2);

    void copyFrom(final IOldErlangProjectProperties bprefs);

    String getExternalIncludesFile();

    void setExternalIncludesFile(final String file);

    void setExternalModulesFile(final String externalModules);

    String getExternalModulesFile();

    RuntimeInfo getRuntimeInfo();

    RuntimeVersion getRuntimeVersion();

    void preferenceChange(final PreferenceChangeEvent event);

    void setRuntimeVersion(final RuntimeVersion runtimeVersion);

}
