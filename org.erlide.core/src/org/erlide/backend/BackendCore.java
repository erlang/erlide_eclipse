package org.erlide.backend;

import org.eclipse.core.resources.IProject;
import org.erlide.backend.internal.BackendFactory;
import org.erlide.backend.internal.BackendManager;
import org.erlide.backend.runtimeinfo.RuntimeInfoPreferencesSerializer;
import org.erlide.launch.EpmdWatchJob;
import org.erlide.runtime.HostnameUtils;
import org.erlide.runtime.epmd.EpmdWatcher;
import org.erlide.runtime.epmd.IEpmdListener;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.runtimeinfo.RuntimeInfoCatalog;
import org.erlide.runtime.runtimeinfo.RuntimeInfoManagerData;

public class BackendCore {

    private static RuntimeInfoCatalog runtimeInfoCatalog;
    private static IBackendManager backendManager;
    private static BackendFactory backendFactory;
    private static EpmdWatcher epmdWatcher;
    private static EpmdWatchJob epmdWatcherJob;

    public static final RuntimeInfoCatalog getRuntimeInfoCatalog() {
        if (runtimeInfoCatalog == null) {
            final RuntimeInfoPreferencesSerializer serializer = new RuntimeInfoPreferencesSerializer();
            final RuntimeInfoManagerData data = serializer.load();

            runtimeInfoCatalog = new RuntimeInfoCatalog();
            runtimeInfoCatalog.setRuntimes(data.runtimes,
                    data.defaultRuntimeName, data.erlideRuntimeName);
            HostnameUtils.detectHostNames(runtimeInfoCatalog.erlideRuntime);
        }
        return runtimeInfoCatalog;
    }

    public static final IBackendManager getBackendManager() {
        if (backendManager == null) {
            final RuntimeInfo erlideRuntime = getRuntimeInfoCatalog()
                    .getErlideRuntime();
            backendFactory = new BackendFactory(getRuntimeInfoCatalog());
            backendManager = new BackendManager(erlideRuntime, backendFactory);
        }
        return backendManager;
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
