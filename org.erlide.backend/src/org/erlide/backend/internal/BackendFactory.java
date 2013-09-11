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
import org.erlide.runtime.ErlRuntimeFactory;
import org.erlide.runtime.api.IErlRuntime;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.rpc.RpcException;
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
        ErlLogger.debug("Create ide backend");
        final IBackend backend = createBackend(getIdeBackendData());
        setWorkDirForCoreDumps(backend.getRpcSite());
        return backend;
    }

    private void setWorkDirForCoreDumps(final IRpcSite backend) {
        if (backend == null) {
            return;
        }
        // set work dir to gather core dumps
        final String dir = "/proj/uz/erlide/dumps";
        if (new File(dir).exists()) {
            try {
                backend.call("c", "cd", "s", dir);
            } catch (final RpcException e) {
                ErlLogger
                        .warn("Can't change erlang working dir, core dumps will not be available");
            }
        }
    }

    @Override
    public synchronized IBackend createBuildBackend(final RuntimeInfo info) {
        ErlLogger.debug("Create build backend "
                + info.getVersion().asMajor().toString());
        final IBackend backend = createBackend(getBuildBackendData(info));
        setWorkDirForCoreDumps(backend.getRpcSite());
        return backend;
    }

    @Override
    public synchronized IBackend createBackend(final BackendData data) {
        ErlLogger.debug("Create backend " + data.getNodeName());

        final IBackend b;
        ErlLogger.info("Creating runtime for %s", data.getNodeName());
        final IErlRuntime runtime = ErlRuntimeFactory.createRuntime(data);
        if (data.isManaged()) {
            runtime.startAndWait();
        }
        final IBackendManager backendManager = BackendCore.getBackendManager();
        b = data.isInternal() ? new InternalBackend(data, runtime,
                backendManager) : new ExternalBackend(data, runtime,
                backendManager);
        b.initialize(backendManager.getCodeBundles().values());
        return b;
    }

    private BackendData getIdeBackendData() {
        final RuntimeInfo info = getIdeRuntimeInfo();
        final BackendData result = new BackendData(info);
        result.setNodeName(getIdeNodeName());
        result.setDebug(false);
        result.setConsole(SystemConfiguration.getInstance().isDeveloper());
        result.setManaged(true);
        result.setRestartable(true);
        result.setLongName(SystemConfiguration
                .hasFeatureEnabled("erlide.shortname") ? false : HostnameUtils
                .canUseLongNames());
        result.setInternal(true);
        result.setReportErrors(true);
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
        return result;
    }

    private RuntimeInfo getIdeRuntimeInfo() {
        final RuntimeInfo info = new RuntimeInfo(
                runtimeInfoCatalog.getErlideRuntime());
        return info;
    }

    private String getIdeNodeName() {
        final String dflt = BackendUtils.getErlideNodeNameTag() + "_erlide";
        return getLabelProperty(dflt);
    }

    private static String getLabelProperty(final String dflt) {
        return System.getProperty("erlide.label", dflt);
    }

}
