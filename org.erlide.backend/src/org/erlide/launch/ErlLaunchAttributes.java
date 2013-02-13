/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.launch;

import org.erlide.backend.BackendPlugin;

/**
 * @noextend This interface is not intended to be extended by clients.
 * @noimplement This interface is not intended to be implemented by clients.
 */
public interface ErlLaunchAttributes {

    final String NEW_PREFIX = BackendPlugin.PLUGIN_ID;
    final String PREFIX = "org.erlide.core";

    final String PROJECTS = PREFIX + ".projects";

    final String MODULE = PREFIX + ".module";
    final String FUNCTION = PREFIX + ".function";
    final String ARGUMENTS = PREFIX + ".arguments";

    final String RUNTIME_NAME = PREFIX + ".runtime";
    final String DEFAULT_RUNTIME_NAME = "erts";

    final String NODE_NAME = PREFIX + ".nodeName";
    final String COOKIE = PREFIX + ".cookie";
    final String START_ME = PREFIX + ".startMe";
    final String USE_LONG_NAME = PREFIX + ".longName";

    final String DEBUG_FLAGS = PREFIX + ".debugFlags"; // @see
    // IErlDebugConstants

    final String DEBUG_INTERPRET_MODULES = PREFIX + ".interpretModules";

    final String WORKING_DIR = PREFIX + ".working_dir";
    final String DEFAULT_WORKING_DIR = ".";

    final String EXTRA_ARGS = PREFIX + ".extra_args";

    final String CONSOLE = PREFIX + ".console";
    final String INTERNAL = PREFIX + ".has_backend";

    final String DEBUG_TRACED_MODULES = ".traced_modules";

    final String LOAD_ALL_NODES = PREFIX + ".loadAllNodes";

    final String RESTARTABLE = PREFIX + ".restartable";

    final String MANAGED = PREFIX + ".managed";

    final String SHELL = PREFIX + ".startShell";

}
