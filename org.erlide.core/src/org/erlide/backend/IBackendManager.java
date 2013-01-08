package org.erlide.backend;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.backend.ICodeBundle.CodeContext;
import org.erlide.runtime.IRpcSite;
import org.osgi.framework.Bundle;

public interface IBackendManager {

    IBackend getBuildBackend(final IProject project) throws BackendException;

    Set<IBackend> getExecutionBackends(final IProject project);

    IBackend getIdeBackend();

    void addBackendListener(final IBackendListener listener);

    void removeBackendListener(final IBackendListener listener);

    Collection<IBackend> getAllBackends();

    void addBundle(Bundle b, Map<String, CodeContext> paths,
            Collection<Pair<String, String>> inits);

    void forEachBackend(final Procedure1<IBackend> visitor);

    void updateNodeStatus(final String host, final Collection<String> started,
            final Collection<String> stopped);

    void addExecutionBackend(final IProject project, final IBackend b);

    void removeExecutionBackend(final IProject project, final IBackend b);

    void dispose(final IBackend backend);

    void loadCodepathExtensions();

    IRpcSite getByName(final String nodeName);

    void moduleLoaded(final IBackend backend, final IProject project,
            final String moduleName);

    IBackend getBackendForLaunch(final ILaunch launch);

    void terminateBackendsForLaunch(final ILaunch launch);

    void removeBackendsForLaunch(final ILaunch launch);

    IBackend createExecutionBackend(final BackendData data);

    void dispose();

    Map<Bundle, ICodeBundle> getCodeBundles();

}
