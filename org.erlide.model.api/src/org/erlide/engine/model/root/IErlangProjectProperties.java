package org.erlide.engine.model.root;

import java.util.Collection;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.osgi.service.prefs.BackingStoreException;

public interface IErlangProjectProperties {

    void store() throws BackingStoreException;

    Collection<IPath> getIncludeDirs();

    void setIncludeDirs(final Collection<IPath> includeDirs2);

    @Deprecated
    IPath getOutputDir();

    Collection<IPath> getOutputDirs();

    @Deprecated
    void setOutputDir(final IPath dir);

    void setOutputDirs(final Collection<IPath> dirs);

    Collection<IPath> getSourceDirs();

    void setSourceDirs(final Collection<IPath> sourceDirs2);

    void copyFrom(final IErlangProjectProperties erlangProjectProperties);

    String getExternalIncludesFile();

    void setExternalIncludesFile(final String file);

    void setExternalModulesFile(final String externalModules);

    String getExternalModulesFile();

    RuntimeInfo getRuntimeInfo();

    RuntimeVersion getRuntimeVersion();

    void preferenceChange(final PreferenceChangeEvent event);

    void setRuntimeVersion(final RuntimeVersion runtimeVersion);

    boolean isNukeOutputOnClean();

    void setNukeOutputOnClean(final boolean nukeOutputOnClean);

    RuntimeVersion getRequiredRuntimeVersion();

    String getRuntimeName();

}
