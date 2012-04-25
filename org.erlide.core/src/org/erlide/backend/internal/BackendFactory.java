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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.model.IProcess;
import org.erlide.backend.BackendData;
import org.erlide.backend.BackendException;
import org.erlide.backend.BackendUtils;
import org.erlide.backend.IBackend;
import org.erlide.backend.IBackendFactory;
import org.erlide.backend.IErlRuntime;
import org.erlide.backend.runtimeinfo.RuntimeInfo;
import org.erlide.backend.runtimeinfo.RuntimeInfoManager;
import org.erlide.jinterface.ErlLogger;
import org.erlide.utils.SystemUtils;

public class BackendFactory implements IBackendFactory {

    final RuntimeInfoManager runtimeInfoManager;

    public BackendFactory(final RuntimeInfoManager runtimeInfoManager) {
        this.runtimeInfoManager = runtimeInfoManager;
    }

    @Override
    public IBackend createIdeBackend() {
        ErlLogger.debug("Create ide backend");
        return createBackend(getIdeBackendData());
    }

    @Override
    public IBackend createBuildBackend(final RuntimeInfo info) {
        ErlLogger.debug("Create build backend "
                + info.getVersion().asMajor().toString());
        return createBackend(getBuildBackendData(info));
    }

    @Override
    public IBackend createBackend(final BackendData data) {
        ErlLogger.debug("Create backend " + data.getNodeName());
        if (!data.isManaged() && !data.isAutostart()) {
            ErlLogger.info("Not creating backend for %s", data.getNodeName());
            return null;
        }

        final IBackend b;
        try {
            final RuntimeInfo info = data.getRuntimeInfo();
            String nodeName = info.getNodeName();
            final boolean hasHost = nodeName.contains("@");
            nodeName = hasHost ? nodeName : nodeName + "@"
                    + RuntimeInfo.getHost();
            ILaunch launch = data.getLaunch();
            final boolean internal = launch == null;
            if (launch == null) {
                launch = launchPeer(data);
            }
            final IProcess mainProcess = launch.getProcesses().length == 0 ? null
                    : launch.getProcesses()[0];
            final IErlRuntime runtime = new ErlRuntime(nodeName,
                    info.getCookie(), mainProcess);
            b = internal ? new InternalBackend(data, runtime)
                    : new ExternalBackend(data, runtime);
            b.initialize();
            return b;
        } catch (final BackendException e) {
            e.printStackTrace();
        }
        return null;
    }

    private ILaunch launchPeer(final BackendData data) {
        final ILaunchConfiguration launchConfig = data.asLaunchConfiguration();
        try {
            final boolean registerForDebug = data.getLaunch() != null
                    || SystemUtils.getInstance().isDeveloper();
            return launchConfig.launch(ILaunchManager.RUN_MODE,
                    new NullProgressMonitor(), false, registerForDebug);
        } catch (final CoreException e) {
            ErlLogger.error(e);
            return null;
        }
    }

    private BackendData getIdeBackendData() {
        final RuntimeInfo info = getIdeRuntimeInfo();
        final BackendData result = new BackendData(runtimeInfoManager, info);
        result.setDebug(false);
        result.setAutostart(true);
        result.setConsole(false);
        if (SystemUtils.getInstance().isDeveloper()) {
            result.setConsole(true);
        }
        if (SystemUtils.getInstance().isMonitoringIdeBackend()) {
            result.setMonitored(true);
        }
        return result;
    }

    private BackendData getBuildBackendData(final RuntimeInfo info) {
        final RuntimeInfo myinfo = RuntimeInfo.copy(info, false);
        myinfo.setNodeName(info.getVersion().asMajor().toString());
        myinfo.setNodeNameSuffix("_" + BackendUtils.getErlideNodeNameTag());

        final BackendData result = new BackendData(runtimeInfoManager, myinfo);
        result.setCookie("erlide");
        result.setDebug(false);
        result.setAutostart(true);
        result.setConsole(false);
        return result;
    }

    private RuntimeInfo getIdeRuntimeInfo() {
        final RuntimeInfo info = RuntimeInfo.copy(
                runtimeInfoManager.getErlideRuntime(), false);
        if (info != null) {
            final String dflt = BackendUtils.getErlideNodeNameTag() + "_erlide";
            final String defLabel = getLabelProperty(dflt);
            info.setNodeName(defLabel);
            info.setCookie("erlide");
        }
        return info;
    }

    private static String getLabelProperty(final String dflt) {
        return System.getProperty("erlide.label", dflt);
    }

}
