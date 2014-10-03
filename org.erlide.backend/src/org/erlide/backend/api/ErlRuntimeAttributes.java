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
package org.erlide.backend.api;

/**
 * @noextend This interface is not intended to be extended by clients.
 * @noimplement This interface is not intended to be implemented by clients.
 */
public class ErlRuntimeAttributes {
    // FIXME this is kind of an indirect dep on core plugin (needs to be
    // started)
    public static final String PREFIX = "org.erlide.core";

    public static final String PROJECTS = PREFIX + ".projects";

    public static final String MODULE = PREFIX + ".module";
    public static final String FUNCTION = PREFIX + ".function";
    public static final String ARGUMENTS = PREFIX + ".arguments";

    public static final String RUNTIME_NAME = PREFIX + ".runtime";
    public static final String DEFAULT_RUNTIME_NAME = "erts";

    public static final String NODE_NAME = PREFIX + ".nodeName";
    public static final String COOKIE = PREFIX + ".cookie";
    public static final String START_ME = PREFIX + ".startMe";
    public static final String USE_LONG_NAME = PREFIX + ".longName";

    public static final String DEBUG_FLAGS = PREFIX + ".debugFlags"; // @see
    // IErlDebugConstants

    public static final String DEBUG_INTERPRET_MODULES = PREFIX + ".interpretModules";

    public static final String WORKING_DIR = PREFIX + ".working_dir";
    public static final String DEFAULT_WORKING_DIR = ".";

    public static final String EXTRA_ARGS = PREFIX + ".extra_args";

    public static final String CONSOLE = PREFIX + ".console";
    public static final String INTERNAL = PREFIX + ".has_backend";

    public static final String DEBUG_TRACED_MODULES = ".traced_modules";
    public static final String LOAD_ALL_NODES = PREFIX + ".loadAllNodes";

    public static final String RESTARTABLE = PREFIX + ".restartable";
    public static final String MANAGED = PREFIX + ".managed";
    public static final String SHELL = PREFIX + ".startShell";

}
