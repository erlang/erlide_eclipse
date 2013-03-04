package org.erlide.backend;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.runtime.ICodeBundle;
import org.erlide.runtime.IRpcSite;
import org.erlide.runtime.RuntimeVersion;
import org.osgi.framework.Bundle;

public interface IBackendManager {

    IBackend getIdeBackend();

    IBackend getBuildBackend(final IProject project) throws BackendException;

    Set<IBackend> getExecutionBackends(final IProject project);

    Collection<IBackend> getAllBackends();

    void addBackendListener(final IBackendListener listener);

    void removeBackendListener(final IBackendListener listener);

    void forEachBackend(final Procedure1<IBackend> visitor);

    IBackend createExecutionBackend(final BackendData data);

    void addExecutionBackend(final IProject project, final IBackend b);

    void removeExecutionBackend(final IProject project, final IBackend b);

    IBackend getBackendForLaunch(final ILaunch launch);

    void terminateBackendsForLaunch(final ILaunch launch);

    void removeBackendsForLaunch(final ILaunch launch);

    void dispose();

    Map<Bundle, ICodeBundle> getCodeBundles();

    IRpcSite getByName(final String nodeName);

    IRpcSite getByVersion(RuntimeVersion version);

    IRpcSite getByProject(String name);

    IRpcSite getByProject(IProject project);

    void loadCodepathExtensions();

    void moduleLoaded(final IBackend backend, final IProject project,
            final String moduleName);

}
