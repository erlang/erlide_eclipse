package org.erlide.core.backend;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.debug.core.ILaunch;
import org.erlide.core.backend.ICodeBundle.CodeContext;
import org.erlide.core.common.Tuple;
import org.erlide.core.rpc.IRpcCallSite;
import org.erlide.jinterface.epmd.EpmdWatcher;
import org.osgi.framework.Bundle;

public interface IBackendManager {

    IBackend getBuildBackend(final IProject project) throws BackendException;

    Set<IBackend> getExecutionBackends(final IProject project);

    IBackend getIdeBackend();

    void addBackendListener(final IBackendListener listener);

    void removeBackendListener(final IBackendListener listener);

    Collection<IBackend> getAllBackends();

    void addBundle(final Bundle b,
            final Collection<Tuple<String, CodeContext>> paths,
            final Collection<Tuple<String, String>> inits);

    void forEachBackend(final IErlideBackendVisitor visitor);

    void updateNodeStatus(final String host, final Collection<String> started,
            final Collection<String> stopped);

    void addExecutionBackend(final IProject project, final IBackend b);

    void removeExecutionBackend(final IProject project, final IBackend b);

    EpmdWatcher getEpmdWatcher();

    void dispose(final IBackend backend);

    void loadCodepathExtensions();

    IRpcCallSite getByName(final String nodeName);

    void moduleLoaded(final IBackend b, final IProject project,
            final String moduleName);

    IBackend getBackendForLaunch(final ILaunch launch);

    void terminateBackendsForLaunch(final ILaunch launch);

    void removeBackendsForLaunch(final ILaunch launch);

    IBackend createExecutionBackend(final BackendData data);

    void dispose();

    Map<Bundle, ICodeBundle> getCodeBundles();

}
