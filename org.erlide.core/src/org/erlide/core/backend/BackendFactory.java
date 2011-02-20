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
package org.erlide.core.backend;

import org.erlide.backend.Backend;
import org.erlide.backend.BackendCore;
import org.erlide.backend.BackendUtil;
import org.erlide.backend.runtime.RuntimeInfo;
import org.erlide.backend.util.BackendUtils;
import org.erlide.common.CommonUtils;
import org.erlide.jinterface.util.ErlLogger;

public class BackendFactory {

    final BackendManager backendManager;

    public BackendFactory(final BackendManager backendManager) {
        this.backendManager = backendManager;
    }

    public Backend createIdeBackend() {
        ErlLogger.debug("Create ide backend");
        return createBackend(getIdeBackendData());
    }

    public Backend createBuildBackend(final RuntimeInfo info) {
        ErlLogger.debug("Create build backend "
                + info.getVersion().asMajor().toString());
        return createBackend(getBuildBackendData(info));
    }

    public Backend createBackend(final BackendData data) {
        ErlLogger.debug("Create backend " + data.getNodeName());
        if (!data.isManaged() && !data.isAutostart()) {
            ErlLogger.info("Not creating backend for %s", data.getNodeName());
            return null;
        }

        final Backend b;
        // try {
        // b = new Backend(data);
        // b.launchRuntime(data);
        // b.initialize();
        // return b;
        // } catch (final BackendException e) {
        // e.printStackTrace();
        // }
        return null;
    }

    private BackendData getIdeBackendData() {
        final RuntimeInfo info = getIdeRuntimeInfo();
        final BackendData result = new BackendData(info);
        result.setDebug(false);
        result.setAutostart(true);
        result.setConsole(false);
        if (CommonUtils.isDeveloper()) {
            result.setConsole(true);
        }
        if ("true".equals(System.getProperty("erlide.monitor.ide"))) {
            result.setMonitored(true);
        }
        return result;
    }

    private BackendData getBuildBackendData(final RuntimeInfo info) {
        final RuntimeInfo myinfo = RuntimeInfo.copy(info, false);
        myinfo.setNodeName(info.getVersion().asMajor().toString());
        myinfo.setNodeNameSuffix("_" + BackendUtils.getErlideNodeNameTag());

        final BackendData result = new BackendData(myinfo);
        result.setCookie("erlide");
        result.setDebug(false);
        result.setAutostart(true);
        result.setConsole(false);
        return result;
    }

    private RuntimeInfo getIdeRuntimeInfo() {
        final RuntimeInfo info = RuntimeInfo.copy(BackendCore
                .getRuntimeInfoManager().getErlideRuntime(), false);
        if (info != null) {
            final String defLabel = BackendUtil.getLabelProperty();
            if (defLabel != null) {
                info.setNodeName(defLabel);
            } else {
                final String nodeName = BackendUtils.getErlideNodeNameTag()
                        + "_erlide";
                info.setNodeName(nodeName);
            }
            info.setCookie("erlide");
        }
        return info;
    }

}
