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

	String SOURCE_DIRS = "source_dirs";
	String DEFAULT_SOURCE_DIRS = "src";

	String INCLUDE_DIRS = "include_dirs";
	String DEFAULT_INCLUDE_DIRS = "include";

	String OUTPUT_DIR = "output_dir";
	String DEFAULT_OUTPUT_DIR = "ebin";

	String EXTERNAL_INCLUDES = "external_includes";
	String DEFAULT_EXTERNAL_INCLUDES = "";

	String RUNTIME_NAME = "backend_name";
	String DEFAULT_RUNTIME_NAME = "erts";

	String COOKIE = "backend_cookie";
	String DEFAULT_COOKIE = "";

	String NODE_NAME = "backend_node";
	String DEFAULT_NODENAME = "erts";

	String USE_PATHZ = "use_pathz";
	String DEFAULT_USE_PATHZ = "true";

	String PROJECT_EXTERNAL_MODULES = "external_modules";
	String DEFAULT_EXTERNAL_MODULES = "";

	String MK_UNIQUE = "make_unique";

}
