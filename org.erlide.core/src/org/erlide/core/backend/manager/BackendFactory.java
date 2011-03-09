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
package org.erlide.core.backend.manager;

import org.erlide.core.backend.Backend;
import org.erlide.core.backend.BackendCore;
import org.erlide.core.backend.BackendData;
import org.erlide.core.backend.BackendException;
import org.erlide.core.backend.runtimeinfo.RuntimeInfo;
import org.erlide.core.common.CommonUtils;
import org.erlide.core.internal.backend.BackendUtil;
import org.erlide.core.internal.backend.BackendUtils;
import org.erlide.jinterface.ErlLogger;

public class BackendFactory {

    public BackendFactory() {
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
        try {
            b = new Backend(data);
            b.launchRuntime(data);
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

    // private void createIdeBackend_old() throws BackendException {
    // final RuntimeInfo info = RuntimeInfo.copy(BackendCore
    // .getRuntimeInfoManager().getErlideRuntime(), false);
    // if (info != null) {
    // final String defLabel = BackendUtil.getLabelProperty();
    // if (defLabel != null) {
    // info.setNodeName(defLabel);
    // } else {
    // final String nodeName = org.erlide.core.backend.internal.BackendUtils
    // .getErlideNodeNameTag() + "_erlide";
    // info.setNodeName(nodeName);
    // }
    // info.setCookie("erlide");
    // info.setHasConsole(CommonUtils.isDeveloper());
    // ErlLogger.debug("creating IDE backend %s", info.getName());
    // final EnumSet<BackendOptions> options = EnumSet.of(
    // BackendOptions.AUTOSTART, BackendOptions.INTERNAL,
    // BackendOptions.IDE);
    // if (!CommonUtils.isDeveloper()) {
    // options.add(BackendOptions.NO_CONSOLE);
    // }
    // return createInternalBackend_old(info, options, null);
    // } else {
    // ErlLogger.error("There is no erlideRuntime defined! "
    // + "Could not start IDE backend.");
    // }
    // }
    //
    // public Backend createBackend_old(final RuntimeInfo info,
    // final Set<BackendOptions> options, final ILaunch launch,
    // final Map<String, String> env) throws BackendException {
    // final String nodeName = info.getNodeName();
    // final boolean exists = EpmdWatcher.findRunningNode(nodeName);
    // Backend b = null;
    //
    // final boolean isRemoteNode = nodeName.contains("@");
    // boolean watch = true;
    // if (exists || isRemoteNode) {
    // ErlLogger.debug("create standalone " + options + " backend '"
    // + info + "' " + Thread.currentThread());
    // b = new Backend(info);
    // watch = false;
    // } else if (options.contains(BackendOptions.AUTOSTART)) {
    // ErlLogger.debug("create managed " + options + " backend '" + info
    // + "' " + Thread.currentThread());
    // b = new Backend(info);
    //
    // final ManagedLauncher launcher = new ManagedLauncher(launch, info,
    // env);
    // final IStreamsProxy streamsProxy = launcher.getStreamsProxy();
    // b.setStreamsProxy(streamsProxy);
    // b.setManaged(true);
    // }
    // if (b == null) {
    // ErlLogger.error("Node %s not found, could not launch!", nodeName);
    // return null;
    // }
    // b.setLaunch(launch);
    // if (launch != null) {
    // DebugPlugin.getDefault().getLaunchManager().addLaunchListener(b);
    // }
    // try {
    // initializeBackend(options, b, watch);
    // } catch (final IOException e) {
    // ErlLogger.error(e);
    // // throw new BackendException(e);
    // }
    // return b;
    // }
    //
    // private Backend createInternalBackend_old(final RuntimeInfo info,
    // final Set<BackendOptions> options, final Map<String, String> env)
    // throws BackendException {
    // final ILaunchConfiguration launchConfig = getLaunchConfiguration(info,
    // options);
    // ILaunch launch;
    // try {
    // launch = launchConfig.launch(ILaunchManager.RUN_MODE,
    // new NullProgressMonitor(), false, false);
    // } catch (final CoreException e) {
    // e.printStackTrace();
    // return null;
    // }
    // final Backend b = createBackend_old(info, options, launch, env);
    // return b;
    // }
    //
    // public Backend getBuildBackend(final IProject project)
    // throws BackendException {
    // final IErlProject erlProject = ErlangCore.getModel().getErlangProject(
    // project);
    // if (erlProject == null) {
    // return null;
    // }
    // final RuntimeInfo info = erlProject.getRuntimeInfo();
    // if (info == null) {
    // ErlLogger.info("Project %s has no runtime info, using ide",
    // project.getName());
    // if (ideBackend == null) {
    // throw new BackendException(
    // "IDE backend is not created - check configuration!");
    // }
    // ideBackend.addProjectPath(project);
    // return ideBackend;
    // }
    // final String version = info.getVersion().asMajor().toString();
    // Backend b = buildBackends.get(version);
    // if (b == null) {
    // info.setNodeName(version);
    // info.setNodeNameSuffix("_"
    // + org.erlide.core.backend.internal.BackendUtils
    // .getErlideNodeNameTag());
    // info.setCookie("erlide");
    // info.setHasConsole(false);
    // // will add workspace unique id
    // final EnumSet<BackendOptions> options = EnumSet.of(
    // BackendOptions.AUTOSTART, BackendOptions.NO_CONSOLE,
    // BackendOptions.INTERNAL);
    // b = createInternalBackend(info, options, null);
    // buildBackends.put(version, b);
    // }
    // return b;
    // }
}
