package org.erlide.core.model.root;

import java.util.Collection;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.erlide.backend.runtimeinfo.RuntimeInfo;
import org.osgi.service.prefs.BackingStoreException;

import com.ericsson.otp.erlang.RuntimeVersion;

public interface IOldErlangProjectProperties {

    void store() throws BackingStoreException;

    Collection<IPath> getIncludeDirs();

    void setIncludeDirs(final Collection<IPath> includeDirs2);

    @Deprecated
    IPath getOutputDir();

    Collection<IPath> getOutputDirs();

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
