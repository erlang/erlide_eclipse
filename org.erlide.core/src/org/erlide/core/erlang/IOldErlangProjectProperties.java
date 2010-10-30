package org.erlide.core.erlang;

import java.util.Collection;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.erlide.jinterface.backend.RuntimeInfo;
import org.erlide.jinterface.backend.RuntimeVersion;

public interface IOldErlangProjectProperties {

    public abstract void load(final IEclipsePreferences node);

    public abstract void store(final IEclipsePreferences node);

    public abstract Collection<IPath> getIncludeDirs();

    public abstract void setIncludeDirs(final Collection<IPath> includeDirs2);

    public abstract IPath getOutputDir();

    public abstract void setOutputDir(final IPath dir);

    public abstract Collection<IPath> getSourceDirs();

    public abstract void setSourceDirs(final Collection<IPath> sourceDirs2);

    public abstract void copyFrom(final IOldErlangProjectProperties bprefs);

    public abstract String getExternalIncludesFile();

    public abstract void setExternalIncludesFile(final String file);

    public abstract void setExternalModulesFile(final String externalModules);

    public abstract String getExternalModulesFile();

    public abstract RuntimeInfo getRuntimeInfo();

    public abstract boolean hasSourceDir(final IPath fullPath);

    public abstract RuntimeVersion getRuntimeVersion();

    public abstract void preferenceChange(final PreferenceChangeEvent event);

    public abstract void setRuntimeVersion(final RuntimeVersion runtimeVersion);

}