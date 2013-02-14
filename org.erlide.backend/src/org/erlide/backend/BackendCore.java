package org.erlide.backend;

import org.eclipse.core.resources.IProject;
import org.erlide.backend.internal.BackendFactory;
import org.erlide.backend.internal.BackendManager;
import org.erlide.backend.runtimeinfo.RuntimeInfoPreferencesSerializer;
import org.erlide.launch.EpmdWatchJob;
import org.erlide.runtime.RuntimeCore;
import org.erlide.runtime.epmd.EpmdWatcher;
import org.erlide.runtime.epmd.IEpmdListener;
import org.erlide.runtime.runtimeinfo.IRuntimeInfoCatalog;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;

public class BackendCore {

    private static IBackendManager backendManager;
    private static BackendFactory backendFactory;
    private static EpmdWatcher epmdWatcher;
    private static EpmdWatchJob epmdWatcherJob;

    public static final IBackendManager getBackendManager() {
        if (backendManager == null) {
            final IRuntimeInfoCatalog catalog = BackendCore
                    .getRuntimeInfoCatalog();
            final RuntimeInfo erlideRuntime = catalog.getErlideRuntime();
            backendFactory = new BackendFactory(catalog);
            backendManager = new BackendManager(erlideRuntime, backendFactory);
        }
        return backendManager;
    }

    public static IRuntimeInfoCatalog getRuntimeInfoCatalog() {
        return RuntimeCore
                .getRuntimeInfoCatalog(new RuntimeInfoPreferencesSerializer());
    }

    public static IBackend getBuildOrIdeBackend(final IProject project) {
        final IBackendManager manager = getBackendManager();
        if (project != null) {
            try {
                return manager.getBuildBackend(project);
            } catch (final BackendException e) {
            }
        }
        return manager.getIdeBackend();
    }

    public static EpmdWatcher getEpmdWatcher() {
        if (epmdWatcher == null) {
            // tryStartEpmdProcess();
            startEpmdWatcher();
        }

        return epmdWatcher;
    }

    private static void startEpmdWatcher() {
        epmdWatcher = new EpmdWatcher();
        epmdWatcher.addEpmdListener((IEpmdListener) getBackendManager());
        epmdWatcherJob = new EpmdWatchJob(epmdWatcher);
        epmdWatcherJob.schedule(1000);
    }

}
