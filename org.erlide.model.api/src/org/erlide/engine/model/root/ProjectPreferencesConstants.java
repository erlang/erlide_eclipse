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
package org.erlide.engine.model.root;

import org.erlide.runtime.runtimeinfo.RuntimeVersion;

/**
 * Constants used in project and plugin preferences
 *
 * @author Eric Merritt [cyberlync at yahoo dot com]
 */
public final class ProjectPreferencesConstants {

    public static final String SOURCE_DIRS = "source_dirs";
    public static final String DEFAULT_SOURCE_DIRS = "src";

    public static final String TEST_DIRS = "test_dirs";
    public static final String DEFAULT_TEST_DIRS = "test";

    public static final String INCLUDE_DIRS = "include_dirs";
    public static final String DEFAULT_INCLUDE_DIRS = "include";

    public static final String OUTPUT_DIR = "output_dir";
    public static final String DEFAULT_OUTPUT_DIR = "ebin";

    public static final String EXTERNAL_INCLUDES = "external_includes";
    public static final String DEFAULT_EXTERNAL_INCLUDES = "";

    public static final String RUNTIME_VERSION = "backend_version";
    public static final RuntimeVersion DEFAULT_RUNTIME_VERSION = new RuntimeVersion(16);
    public static final RuntimeVersion[] SUPPORTED_VERSIONS = { new RuntimeVersion(15),
            new RuntimeVersion(16), new RuntimeVersion(17) };
    public static final RuntimeVersion FALLBACK_RUNTIME_VERSION = SUPPORTED_VERSIONS[0];

    public static final String PROJECT_EXTERNAL_MODULES = "external_modules";
    public static final String DEFAULT_EXTERNAL_MODULES = "";

    public static final String BUILDER = "builder";
    public static final String BUILDER_COMPILE_TARGET = "builderCompileTarget";
    public static final String BUILDER_CLEAN_TARGET = "builderCleanTarget";

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

    private ProjectPreferencesConstants() {
    }

}
