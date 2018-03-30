package org.erlide.backend;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.backend.api.IBackendManager;
import org.erlide.backend.internal.EpmdWatchJob;
import org.erlide.backend.runtimeinfo.RuntimeInfoPreferencesSerializer;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.runtime.api.RuntimeCore;
import org.erlide.runtime.epmd.EpmdWatcher;
import org.erlide.runtime.rpc.IOtpRpc;
import org.erlide.runtime.runtimeinfo.IRuntimeInfoCatalog;

public class BackendCore {

    private static IBackendManager backendManager;
    private static EpmdWatcher epmdWatcher;
    private static EpmdWatchJob epmdWatcherJob;

    public static void init(final IBackendManager aBackendManager) {
        BackendCore.backendManager = aBackendManager;
    }

    public static final IBackendManager getBackendManager() {
        return BackendCore.backendManager;
    }

    public static IRuntimeInfoCatalog getRuntimeInfoCatalog() {
        return RuntimeCore.getRuntimeInfoCatalog(new RuntimeInfoPreferencesSerializer());
    }

    public static IOtpRpc getBuildBackend(@NonNull final IErlProject project) {
        final IBackendManager manager = BackendCore.getBackendManager();
        return manager.getBuildBackend(project).getOtpRpc();
    }

    public static EpmdWatcher getEpmdWatcher() {
        if (BackendCore.epmdWatcher == null) {
            // tryStartEpmdProcess();
            BackendCore.startEpmdWatcher();
        }

        return BackendCore.epmdWatcher;
    }

    private static void startEpmdWatcher() {
        BackendCore.epmdWatcher = new EpmdWatcher();
        BackendCore.epmdWatcher.addEpmdListener(BackendCore.getBackendManager());
        BackendCore.epmdWatcherJob = new EpmdWatchJob(BackendCore.epmdWatcher);
        BackendCore.epmdWatcherJob.schedule(1000);
    }

    public static void stop() {
        if (BackendCore.epmdWatcherJob != null) {
            BackendCore.epmdWatcherJob.stop();
        }
    }

}
