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
package org.erlide.runtime;

/**
 * Constants used in project and plugin preferences
 * 
 * @author Eric Merritt [cyberlync at yahoo dot com]
 */
public class ProjectPreferencesConstants {

	public static final String SOURCE_DIRS = "source_dirs";
	public static final String DEFAULT_SOURCE_DIRS = "src";

	public static final String INCLUDE_DIRS = "include_dirs";
	public static final String DEFAULT_INCLUDE_DIRS = "include";

	public static final String OUTPUT_DIR = "output_dir";
	public static final String DEFAULT_OUTPUT_DIR = "ebin";

	public static final String EXTERNAL_INCLUDES = "external_includes";
	public static final String DEFAULT_EXTERNAL_INCLUDES = "";

	public static final String RUNTIME_NAME = "backend_name";
	public static final String DEFAULT_RUNTIME_NAME = "erlide";

	public static final String COOKIE = "backend_cookie";
	public static final String DEFAULT_COOKIE = "";

	public static final String NODE_NAME = "backend_node";
	public static final String DEFAULT_NODENAME = "erlide";

	public static final String USE_PATHZ = "use_pathz";
	public static final String DEFAULT_USE_PATHZ = "false";

	public static final String PROJECT_EXTERNAL_MODULES = "external_modules";
	public static final String DEFAULT_EXTERNAL_MODULES = "";

	public static final String MK_UNIQUE = "make_unique";

	public static final String PROJECT_TYPE = "type";
	public static final String DEFAULT_PROJECT_TYPE = "NORMAL";

	private ProjectPreferencesConstants() {
	}

}
