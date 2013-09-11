package org.erlide.backend.api;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.epmd.IEpmdListener;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.osgi.framework.Bundle;

public interface IBackendManager extends IEpmdListener {

    IBackend getIdeBackend();

    IBackend getBuildBackend(final IProject project);

    Set<IBackend> getExecutionBackends(final IProject project);

    Collection<IBackend> getAllBackends();

    void addBackendListener(final IBackendListener listener);

    void removeBackendListener(final IBackendListener listener);

    void forEachBackend(final Procedure1<IBackend> visitor);

    IBackend createExecutionBackend(final BackendData data);

    void addExecutionBackend(final IProject project, final IBackend b);

    void removeExecutionBackend(final IProject project,
            final IProjectCodeLoader b);

    IPluginCodeLoader getBackendForLaunch(final ILaunch launch);

    void terminateBackendsForLaunch(final ILaunch launch);

    void removeBackendsForLaunch(final ILaunch launch);

    void dispose();

    Map<Bundle, ICodeBundle> getCodeBundles();

    IRpcSite getByName(final String nodeName);

    IRpcSite getByVersion(RuntimeVersion version);

    IRpcSite getByProject(String projectName);

    void loadCodepathExtensions();

    void moduleLoaded(final IBackend backend, final IProject project,
            final String moduleName);

    IPluginCodeLoader getByProcess(IProcess ertsProcess);

}
