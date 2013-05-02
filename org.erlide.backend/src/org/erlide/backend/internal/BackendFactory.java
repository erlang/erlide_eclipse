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

import org.eclipse.debug.core.model.IProcess;
import org.erlide.backend.BackendCore;
import org.erlide.backend.BackendData;
import org.erlide.backend.BackendException;
import org.erlide.backend.BackendUtils;
import org.erlide.backend.IBackend;
import org.erlide.backend.IBackendFactory;
import org.erlide.backend.IBackendManager;
import org.erlide.runtime.IErlRuntime;
import org.erlide.runtime.IRpcSite;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.runtime.runtimeinfo.IRuntimeInfoCatalog;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.util.ErlLogger;
import org.erlide.util.HostnameUtils;
import org.erlide.util.IProvider;
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
        if (!data.isManaged()) {
            ErlLogger.info("Not creating backend for %s", data.getNodeName());
            // return null;
        }

        final IBackend b;
        try {
            final String nodeName = data.getQualifiedNodeName();
            final IProvider<IProcess> erlProcessProvider = new LaunchBeamProcessProvider(
                    data);
            final IErlRuntime runtime = data.getRuntimeInfo() == null ? new NullErlRuntime()
                    : new ErlRuntime(nodeName, data.getCookie(),
                            erlProcessProvider, !data.isReportErrors(),
                            data.hasLongName(), data.isInternal());
            final IBackendManager backendManager = BackendCore
                    .getBackendManager();
            b = data.isInternal() ? new InternalBackend(data, runtime,
                    backendManager) : new ExternalBackend(data, runtime,
                    backendManager);
            b.initialize();
            return b;
        } catch (final BackendException e) {
            e.printStackTrace();
        }
        return null;
    }

    private BackendData getIdeBackendData() {
        final RuntimeInfo info = getIdeRuntimeInfo();
        final BackendData result = new BackendData(info);
        result.setNodeName(getIdeNodeName());
        result.setDebug(false);
        result.setConsole(false);
        result.setRestartable(true);
        result.setLongName(SystemConfiguration
                .hasFeatureEnabled("erlide.shortname") ? false : HostnameUtils
                .canUseLongNames());
        if (SystemConfiguration.getInstance().isDeveloper()) {
            result.setConsole(true);
        }
        result.setInternal(true);
        result.debugPrint();
        return result;
    }

    private BackendData getBuildBackendData(final RuntimeInfo info) {
        final RuntimeInfo myinfo = RuntimeInfo.copy(info);

        final BackendData result = new BackendData(myinfo);
        result.setNodeName(info.getVersion().asMajor().toString() + "_"
                + BackendUtils.getErlideNodeNameTag());
        result.setCookie("erlide");
        result.setRestartable(true);
        result.setDebug(false);
        result.setConsole(false);
        result.setLongName(HostnameUtils.canUseLongNames());
        result.setInternal(true);
        return result;
    }

    private RuntimeInfo getIdeRuntimeInfo() {
        final RuntimeInfo info = RuntimeInfo.copy(runtimeInfoCatalog
                .getErlideRuntime());
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
