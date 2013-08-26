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
package org.erlide.runtime.api;

/**
 * @noextend This interface is not intended to be extended by clients.
 * @noimplement This interface is not intended to be implemented by clients.
 */
public interface ErlRuntimeAttributes {

    String PREFIX = "org.erlide.core";

    String PROJECTS = PREFIX + ".projects";

    String MODULE = PREFIX + ".module";
    String FUNCTION = PREFIX + ".function";
    String ARGUMENTS = PREFIX + ".arguments";

    String RUNTIME_NAME = PREFIX + ".runtime";
    String DEFAULT_RUNTIME_NAME = "erts";

    String NODE_NAME = PREFIX + ".nodeName";
    String COOKIE = PREFIX + ".cookie";
    String START_ME = PREFIX + ".startMe";
    String USE_LONG_NAME = PREFIX + ".longName";

    String DEBUG_FLAGS = PREFIX + ".debugFlags"; // @see
    // IErlDebugConstants

    String DEBUG_INTERPRET_MODULES = PREFIX + ".interpretModules";

    String WORKING_DIR = PREFIX + ".working_dir";
    String DEFAULT_WORKING_DIR = ".";

    String EXTRA_ARGS = PREFIX + ".extra_args";

    String CONSOLE = PREFIX + ".console";
    String INTERNAL = PREFIX + ".has_backend";

    String DEBUG_TRACED_MODULES = ".traced_modules";

    String LOAD_ALL_NODES = PREFIX + ".loadAllNodes";

    String RESTARTABLE = PREFIX + ".restartable";

    String MANAGED = PREFIX + ".managed";

    String SHELL = PREFIX + ".startShell";

}
