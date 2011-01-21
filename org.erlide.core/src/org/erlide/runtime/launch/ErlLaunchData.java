/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.launch;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.debug.ErlDebugConstants;

import com.google.common.collect.Maps;

public class ErlLaunchData {

    public List<String> interpretedModules;
    public final int debugFlags;
    public final String xtraArgs;
    public final String workingDir;
    public final boolean startMe;
    public final boolean longName;
    public final String cookie;
    public final String nodeName;
    public final String runtime;
    public final String args;
    public final String function;
    public final String module;
    public final String[] projectNames;
    public final String prjs;
    public final boolean console;
    public final boolean isInternal;
    public final boolean loadAllNodes;
    public final Map<String, String> env;

    @SuppressWarnings("unchecked")
    public ErlLaunchData(final ILaunchConfiguration config,
            final boolean internal) throws CoreException {
        prjs = config.getAttribute(ErlLaunchAttributes.PROJECTS, "").trim();
        projectNames = prjs.length() == 0 ? new String[] {} : prjs.split(";");
        module = config.getAttribute(ErlLaunchAttributes.MODULE, "").trim();
        function = config.getAttribute(ErlLaunchAttributes.FUNCTION, "").trim();
        args = config.getAttribute(ErlLaunchAttributes.ARGUMENTS, "").trim();
        runtime = config.getAttribute(ErlLaunchAttributes.RUNTIME_NAME, "")
                .trim();
        nodeName = config.getAttribute(ErlLaunchAttributes.NODE_NAME, "")
                .trim();
        cookie = config.getAttribute(ErlLaunchAttributes.COOKIE, "").trim();
        longName = config.getAttribute(ErlLaunchAttributes.USE_LONG_NAME, true);

        startMe = internal
                || config.getAttribute(ErlLaunchAttributes.START_ME, true);
        workingDir = config.getAttribute(ErlLaunchAttributes.WORKING_DIR,
                ErlLaunchAttributes.DEFAULT_WORKING_DIR).trim();
        xtraArgs = config.getAttribute(ErlLaunchAttributes.EXTRA_ARGS, "")
                .trim();
        debugFlags = config.getAttribute(ErlLaunchAttributes.DEBUG_FLAGS,
                ErlDebugConstants.DEFAULT_DEBUG_FLAGS);
        interpretedModules = config.getAttribute(
                ErlLaunchAttributes.DEBUG_INTERPRET_MODULES,
                new ArrayList<String>());
        console = config.getAttribute(ErlLaunchAttributes.CONSOLE, true);
        isInternal = config.getAttribute(ErlLaunchAttributes.INTERNAL, false);
        loadAllNodes = config.getAttribute(ErlLaunchAttributes.LOAD_ALL_NODES,
                false);
        env = config.getAttribute(
                "org.eclipse.debug.core.environmentVariables",
                Maps.newHashMap());
    }

    public void debugPrint(final String mode) {
        ErlLogger.info("About to start a backend in " + mode
                + " mode, with attributes::");
        ErlLogger.info("  projects: " + Arrays.toString(projectNames));
        if (module.length() > 0) {
            ErlLogger.info("  module: " + module);
            ErlLogger.info("  function: " + function);
            ErlLogger.info("  args: " + args);
        }
        ErlLogger.info("  runtime: " + runtime);
        ErlLogger.info("  node name: " + nodeName);
        ErlLogger.info("  long name: " + longName);
        ErlLogger.info("  cookie: " + cookie);
        ErlLogger.info("  workdir: " + workingDir);
        ErlLogger.info("  args: " + xtraArgs);
        ErlLogger.info("  console: " + console);
        ErlLogger.info("  hasBackend: " + isInternal);
        ErlLogger.info("  debugFlags: " + debugFlags);
        ErlLogger.info("  interpretedModules: " + interpretedModules);
        ErlLogger.info("  loadAllNodes: " + loadAllNodes);
        ErlLogger.info("  env: " + env);
        if (startMe) {
            ErlLogger.info("  * start it if not running");
        }
        ErlLogger.info("---------------");
    }

}
