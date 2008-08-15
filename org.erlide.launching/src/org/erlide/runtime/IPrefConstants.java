/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *     Vlad Dumitrescu
 *     Mickaël Rémond
 *******************************************************************************/
package org.erlide.runtime;

/**
 * Constants used in project and plugin preferences
 * 
 * @author Eric Merritt [cyberlync at yahoo dot com]
 */
public interface IPrefConstants {

	String PROJECT_SOURCE_DIRS = "source_dirs";
	String DEFAULT_SOURCE_DIRS = "src";

	String PROJECT_INCLUDE_DIRS = "include_dirs";
	String DEFAULT_INCLUDE_DIRS = "include";

	String PROJECT_OUTPUT_DIR = "output_dir";
	String DEFAULT_OUTPUT_DIR = "ebin";

	String PROJECT_EXTERNAL_INCLUDES = "external_includes";
	String DEFAULT_EXTERNAL_INCLUDES = "";

	String PROJECT_BUILD_BACKEND_NAME = "backend_name";
	String DEFAULT_BUILD_BACKEND_NAME = "";

	String PROJECT_BACKEND_COOKIE = "backend_cookie";
	String DEFAULT_BACKEND_COOKIE = "";

	String PROJECT_USE_PATHZ = "use_pathz";
	String DEFAULT_USE_PATHZ = "true";

	String PROJECT_EXTERNAL_MODULES = "external_modules";
	String DEFAULT_EXTERNAL_MODULES = "";

}
