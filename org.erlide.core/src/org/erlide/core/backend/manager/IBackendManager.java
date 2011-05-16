package org.erlide.core.backend.manager;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.debug.core.ILaunch;
import org.erlide.core.backend.Backend;
import org.erlide.core.backend.BackendData;
import org.erlide.core.backend.BackendException;
import org.erlide.core.backend.BackendListener;
import org.erlide.core.backend.ErlideBackendVisitor;
import org.erlide.core.backend.CodeBundle.CodeContext;
import org.erlide.core.backend.internal.CodeBundleImpl;
import org.erlide.core.common.Tuple;
import org.erlide.core.rpc.RpcCallSite;
import org.erlide.jinterface.epmd.EpmdWatcher;
import org.osgi.framework.Bundle;

public interface IBackendManager {

    Backend getBuildBackend(final IProject project) throws BackendException;

    Set<Backend> getExecutionBackends(final IProject project);

    Backend getIdeBackend();

    void addBackendListener(final BackendListener listener);

    void removeBackendListener(final BackendListener listener);

    Collection<Backend> getAllBackends();

    void addBundle(final Bundle b,
            final Collection<Tuple<String, CodeContext>> paths,
            final Tuple<String, String> init);

    void forEachBackend(final ErlideBackendVisitor visitor);

    void updateNodeStatus(final String host, final Collection<String> started,
            final Collection<String> stopped);

    void addExecutionBackend(final IProject project, final Backend b);

    void removeExecutionBackend(final IProject project, final Backend b);

    EpmdWatcher getEpmdWatcher();

    void dispose(final Backend backend);

    void loadCodepathExtensions();

    RpcCallSite getByName(final String nodeName);

    void moduleLoaded(final Backend b, final IProject project,
            final String moduleName);

    Backend getBackendForLaunch(final ILaunch launch);

    void terminateBackendsForLaunch(final ILaunch launch);

    void removeBackendsForLaunch(final ILaunch launch);

    Backend createExecutionBackend(final BackendData data);

    void dispose();

    Map<Bundle, CodeBundleImpl> getCodeBundles();

}
