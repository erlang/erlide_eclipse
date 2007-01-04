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
package org.erlide.core;

/**
 * Constants used in project and plugin preferences
 * 
 * @author Eric Merritt [cyberlync at yahoo dot com]
 */
public interface IPrefConstants {

	// Erts preferences

	String ERTS_OTP_HOME = "otp_home";

	String DEFAULT_OTP_HOME = "";

	String ERTS_PATH_A = "path_a";

	String DEFAULT_PATH_A = "";

	String ERTS_PATH_Z = "path_z";

	String DEFAULT_PATH_Z = "";

	String ERTS_EXTRA_ARGS = "extra_erts_args";

	String DEFAULT_EXTRA_ARGS = "";

	// /

	String PROJECT_OTP_PROJECT_STRUCTURE = "otp_project_structure";

	boolean DEFAULT_OTP_PROJECT_STRUCTURE = true;

	String PROJECT_SOURCE_DIRS = "source_dirs";

	String DEFAULT_SOURCE_DIRS = "src";

	String PROJECT_INCLUDE_DIRS = "include_dirs";

	String DEFAULT_INCLUDE_DIRS = "include";

	String PROJECT_OUTPUT_DIR = "output_dir";

	String DEFAULT_OUTPUT_DIR = "ebin";

	String PROJECT_EXTERNAL_INCLUDES = "external_includes";

	String DEFAULT_EXTERNAL_INCLUDES = "";

	String PROJECT_BACKEND_NODE_NAME = "backend_name";

	String DEFAULT_BACKEND_NODE_NAME = "";

}
