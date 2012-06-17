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

import org.erlide.core.ErlangCore;

/**
 * @noextend This interface is not intended to be extended by clients.
 * @noimplement This interface is not intended to be implemented by clients.
 */
public interface ErlLaunchAttributes {

    final String prefix = ErlangCore.PLUGIN_ID;

    final String PROJECTS = prefix + ".projects";

    final String MODULE = prefix + ".module";
    final String FUNCTION = prefix + ".function";
    final String ARGUMENTS = prefix + ".arguments";

    final String RUNTIME_NAME = prefix + ".runtime";
    final String DEFAULT_RUNTIME_NAME = "erts";

    final String NODE_NAME = prefix + ".nodeName";
    final String COOKIE = prefix + ".cookie";
    final String START_ME = prefix + ".startMe";
    final String USE_LONG_NAME = prefix + ".longName";

    final String DEBUG_FLAGS = prefix + ".debugFlags"; // @see
    // IErlDebugConstants

    final String DEBUG_INTERPRET_MODULES = prefix + ".interpretModules";

    final String WORKING_DIR = prefix + ".working_dir";
    final String DEFAULT_WORKING_DIR = ".";

    final String EXTRA_ARGS = prefix + ".extra_args";

    final String CONSOLE = prefix + ".console";
    final String INTERNAL = prefix + ".has_backend";

    final String DEBUG_TRACED_MODULES = ".traced_modules";

    final String LOAD_ALL_NODES = prefix + ".loadAllNodes";

    final String AUTOSTART = prefix + ".autostart";

    final String MANAGED = prefix + ".managed";

    final String MONITORED = prefix + ".monitored";

    final String SHELL = prefix + ".startShell";
}
