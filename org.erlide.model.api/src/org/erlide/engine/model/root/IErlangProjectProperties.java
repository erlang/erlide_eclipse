package org.erlide.engine.model.root;

import java.util.Collection;

import org.eclipse.core.runtime.IPath;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.osgi.service.prefs.BackingStoreException;

public interface IErlangProjectProperties {

    void store() throws BackingStoreException;

    void copyFrom(final IErlangProjectProperties erlangProjectProperties);

    Collection<IPath> getIncludeDirs();

    void setIncludeDirs(final Collection<IPath> includeDirs2);

    IPath getOutputDir();

    void setOutputDir(final IPath dir);

    Collection<IPath> getSourceDirs();

    void setSourceDirs(final Collection<IPath> sourceDirs2);

    String getExternalIncludesFile();

    void setExternalIncludesFile(final String file);

    String getExternalModulesFile();

    void setExternalModulesFile(final String externalModules);

    RuntimeInfo getRuntimeInfo();

    @Deprecated
    String getRuntimeName();

    RuntimeVersion getRuntimeVersion();

    void setRuntimeVersion(final RuntimeVersion runtimeVersion);

    RuntimeVersion getRequiredRuntimeVersion();

    boolean isNukeOutputOnClean();

    void setNukeOutputOnClean(final boolean nukeOutputOnClean);

    String getBuilderName();

    void setBuilderName(String builder);

    void setName(String projectName);

    String getName();

    IPath getLocation();

    void setLocation(IPath location);

}
