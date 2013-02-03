/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *     Vlad Dumitrescu
 *     Mickael Remond
 *******************************************************************************/
package org.erlide.core.internal.model.root;

/**
 * Constants used in project and plugin preferences
 * 
 * @author Eric Merritt [cyberlync at yahoo dot com]
 */
public final class ProjectPreferencesConstants {

    public static final String SOURCE_DIRS = "source_dirs";
    public static final String DEFAULT_SOURCE_DIRS = "src";

    public static final String TEST_DIRS = "test_dirs";
    public static final String DEFAULT_TEST_DIRS = "";

    public static final String INCLUDE_DIRS = "include_dirs";
    public static final String DEFAULT_INCLUDE_DIRS = "include";

    public static final String OUTPUT_DIR = "output_dir";
    public static final String DEFAULT_OUTPUT_DIR = "ebin";

    public static final String EXTERNAL_INCLUDES = "external_includes";
    public static final String DEFAULT_EXTERNAL_INCLUDES = "";

    public static final String RUNTIME_VERSION = "backend_version";
    public static final String DEFAULT_RUNTIME_VERSION = "R14B";

    public static final String RUNTIME_NAME = "backend_name";
    public static final String DEFAULT_RUNTIME_NAME = "R14B";

    public static final String PROJECT_EXTERNAL_MODULES = "external_modules";
    public static final String DEFAULT_EXTERNAL_MODULES = "";

    public static final String PROJECT_TYPE = "type";
    public static final String DEFAULT_PROJECT_TYPE = "NORMAL";

    // new project properties

    public static final String SOURCES = "sources";
    public static final String BACKEND_COOKIE = "backendCookie";
    public static final String BACKEND_NODE_NAME = "BackendName";
    public static final String REQUIRED_BACKEND_VERSION = "requiredBackendVersion";
    public static final String OUTPUT = "output";
    public static final String INCLUDES = "includes";
    public static final String DIRECTORY = "directory";
    public static final String PROJECT = "project";

    // tracing properties
    public static final String TRACING = "tracing";
    public static final String TRACED_MODULES = "modules";
    public static final String NUKE_OUTPUT_ON_CLEAN = "nukeOutputOnClean";

    private ProjectPreferencesConstants() {
    }

}
