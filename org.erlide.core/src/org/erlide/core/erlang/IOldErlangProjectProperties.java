package org.erlide.core.erlang;

import java.util.Collection;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.erlide.backend.runtime.RuntimeInfo;
import org.erlide.backend.runtime.RuntimeVersion;

public interface IOldErlangProjectProperties {

    void load(final IEclipsePreferences node);

    void store(final IEclipsePreferences node);

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

    boolean hasSourceDir(final IPath fullPath);

    RuntimeVersion getRuntimeVersion();

    void preferenceChange(final PreferenceChangeEvent event);

    void setRuntimeVersion(final RuntimeVersion runtimeVersion);

}
