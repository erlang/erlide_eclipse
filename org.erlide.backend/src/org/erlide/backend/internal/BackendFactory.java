/*******************************************************************************
 * Copyright (c) 2005-2011 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.backend.internal;

import java.io.File;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.backend.BackendCore;
import org.erlide.backend.BackendUtils;
import org.erlide.backend.api.BackendData;
import org.erlide.backend.api.IBackend;
import org.erlide.backend.api.IBackendFactory;
import org.erlide.backend.api.IBackendManager;
import org.erlide.backend.api.ICodeBundle.CodeContext;
import org.erlide.runtime.ManagedOtpNodeProxy;
import org.erlide.runtime.OtpNodeProxy;
import org.erlide.runtime.api.IOtpNodeProxy;
import org.erlide.runtime.runtimeinfo.IRuntimeInfoCatalog;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.util.ErlLogger;
import org.erlide.util.HostnameUtils;
import org.erlide.util.SystemConfiguration;

public class BackendFactory implements IBackendFactory {

    final IRuntimeInfoCatalog runtimeInfoCatalog;

    public BackendFactory(final IRuntimeInfoCatalog runtimeInfoManager) {
        runtimeInfoCatalog = runtimeInfoManager;
    }

    @Override
    public IBackend createIdeBackend() {
        final BackendData data = getIdeBackendData();
        ErlLogger.debug("Create ide backend " + data.getRuntimeInfo().getVersion());
        final IBackend backend = createBackend(data);
        return backend;
    }

    @Override
    public synchronized IBackend createBuildBackend(final RuntimeInfo info) {
        ErlLogger.debug("Create build backend " + info.getVersion().asMajor().toString());
        final IBackend backend = createBackend(getBuildBackendData(info));
        return backend;
    }

    @Override
    public synchronized IBackend createBackend(final BackendData data) {
        ErlLogger.debug("Create backend " + data.getNodeName());

        final IBackend b;
        final IOtpNodeProxy runtime = createNodeProxy(data);

        final IBackendManager backendManager = BackendCore.getBackendManager();
        b = data.isInternal() ? new InternalBackend(data, runtime, backendManager)
                : new ExternalBackend(data, runtime, backendManager);

        b.initialize(data.getContext(), backendManager.getCodeBundles());
        return b;
    }

    @Override
    @NonNull
    public IOtpNodeProxy createNodeProxy(final BackendData data) {
        IOtpNodeProxy result;
        if (data.isManaged()) {
            result = new ManagedOtpNodeProxy(data);
        } else {
            result = new OtpNodeProxy(data);
        }
        final IOtpNodeProxy runtime = result;
        runtime.startAndWait();
        return runtime;
    }

    private BackendData getIdeBackendData() {
        final RuntimeInfo info = getIdeRuntimeInfo();
        final BackendData result = new BackendData(info);
        result.setNodeName(getIdeNodeName());
        result.setDebug(false);
        result.setConsole(SystemConfiguration.getInstance().isDeveloper());
        result.setManaged(true);
        result.setRestartable(true);
        result.setLongName(SystemConfiguration.hasFeatureEnabled("erlide.shortname") ? false
                : HostnameUtils.canUseLongNames());
        result.setInternal(true);
        result.setReportErrors(true);
        result.setContext(CodeContext.IDE);
        return result;
    }

    private BackendData getBuildBackendData(final @NonNull RuntimeInfo info) {
        final RuntimeInfo myinfo = new RuntimeInfo(info);

        final BackendData result = new BackendData(myinfo);
        result.setNodeName(info.getVersion().asMajor().toString() + "_"
                + BackendUtils.getErlideNodeNameTag());
        result.setCookie("erlide");
        result.setRestartable(true);
        result.setDebug(false);
        result.setManaged(true);
        result.setConsole(false);
        result.setLongName(HostnameUtils.canUseLongNames());
        result.setInternal(true);
        result.setReportErrors(true);
        result.setContext(CodeContext.IDE);
        return result;
    }

    private RuntimeInfo getIdeRuntimeInfo() {
        final RuntimeInfo runtime = runtimeInfoCatalog.getErlideRuntime();
        if (runtime != null && runtimeHomeDirExists(runtime)) {
            return new RuntimeInfo(runtime);
        }
        for (final RuntimeInfo aruntime : runtimeInfoCatalog.getRuntimes()) {
            if (aruntime != null && runtimeHomeDirExists(aruntime)) {
                return new RuntimeInfo(aruntime);
            }
        }
        return null;
    }

    private boolean runtimeHomeDirExists(final RuntimeInfo runtime) {
        if (runtime == null) {
            return false;
        }
        final String otpHome = runtime.getOtpHome();
        return otpHome != null && new File(otpHome).exists();
    }

    private String getIdeNodeName() {
        final String dflt = BackendUtils.getErlideNodeNameTag() + "_erlide";
        return getLabelProperty(dflt);
    }

    private static String getLabelProperty(final String dflt) {
        return System.getProperty("erlide.label", dflt);
    }

}
