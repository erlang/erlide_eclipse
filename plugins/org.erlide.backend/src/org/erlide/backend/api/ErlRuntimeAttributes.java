/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others. All rights reserved. This program and
 * the accompanying materials are made available under the terms of the Eclipse Public
 * License v1.0 which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: Vlad Dumitrescu
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

    public static final String PROJECTS = ErlRuntimeAttributes.PREFIX + ".projects";

    public static final String MODULE = ErlRuntimeAttributes.PREFIX + ".module";
    public static final String FUNCTION = ErlRuntimeAttributes.PREFIX + ".function";
    public static final String ARGUMENTS = ErlRuntimeAttributes.PREFIX + ".arguments";

    public static final String RUNTIME_NAME = ErlRuntimeAttributes.PREFIX + ".runtime";
    public static final String DEFAULT_RUNTIME_NAME = "erts";

    public static final String NODE_NAME = ErlRuntimeAttributes.PREFIX + ".nodeName";
    public static final String COOKIE = ErlRuntimeAttributes.PREFIX + ".cookie";
    public static final String START_ME = ErlRuntimeAttributes.PREFIX + ".startMe";
    public static final String USE_LONG_NAME = ErlRuntimeAttributes.PREFIX + ".longName";

    public static final String DEBUG_FLAGS = ErlRuntimeAttributes.PREFIX + ".debugFlags"; // @see
    // IErlDebugConstants

    public static final String DEBUG_INTERPRET_MODULES = ErlRuntimeAttributes.PREFIX
            + ".interpretModules";

    public static final String WORKING_DIR = ErlRuntimeAttributes.PREFIX + ".working_dir";
    public static final String DEFAULT_WORKING_DIR = ".";

    public static final String EXTRA_ARGS = ErlRuntimeAttributes.PREFIX + ".extra_args";

    public static final String CONSOLE = ErlRuntimeAttributes.PREFIX + ".console";
    public static final String INTERNAL = ErlRuntimeAttributes.PREFIX + ".has_backend";

    public static final String DEBUG_TRACED_MODULES = ".traced_modules";
    public static final String LOAD_ALL_NODES = ErlRuntimeAttributes.PREFIX
            + ".loadAllNodes";

    public static final String RESTARTABLE = ErlRuntimeAttributes.PREFIX + ".restartable";
    public static final String MANAGED = ErlRuntimeAttributes.PREFIX + ".managed";
    public static final String SHELL = ErlRuntimeAttributes.PREFIX + ".startShell";

}
