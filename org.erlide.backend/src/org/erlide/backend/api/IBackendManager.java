package org.erlide.backend.api;

import java.util.Collection;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.epmd.IEpmdListener;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;

public interface IBackendManager extends IEpmdListener {

    IBackend getIdeBackend();

    IBackend getBuildBackend(@NonNull final IErlProject project);

    void removeBackend(IBackend backend);

    Set<IBackend> getExecutionBackends(@NonNull final IProject project);

    Collection<IBackend> getAllBackends();

    void addBackendListener(final IBackendListener listener);

    void removeBackendListener(final IBackendListener listener);

    void forEachBackend(final Procedure1<IBackend> visitor);

    IBackend createExecutionBackend(final BackendData data);

    void addExecutionBackend(final IProject project, final IBackend b);

    void removeExecutionBackend(final IProject project, final IProjectCodeLoader b);

    IPluginCodeLoader getBackendForLaunch(final ILaunch launch);

    void terminateBackendsForLaunch(final ILaunch launch);

    void removeBackendsForLaunch(final ILaunch launch);

    void dispose();

    Collection<ICodeBundle> getCodeBundles();

    IOtpRpc getByName(final String nodeName);

    IOtpRpc getByVersion(RuntimeVersion version);

    IOtpRpc getByProject(String projectName);

    void loadCodepathExtensions();

    void moduleLoaded(final IBackend backend, final IProject project,
            final String moduleName);

    IBackendFactory getFactory();

}
