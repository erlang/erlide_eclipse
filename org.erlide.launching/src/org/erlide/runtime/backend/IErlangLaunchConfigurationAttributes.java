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
package org.erlide.runtime.backend;

import org.erlide.runtime.ErlangLaunchPlugin;

public interface IErlangLaunchConfigurationAttributes {

	String prefix = ErlangLaunchPlugin.PLUGIN_ID;

	String ATTR_ENODE_MODULE = "module";

	String DEFAULT_ENODE_MODULE = "erlang";

	String ATTR_ENODE_FUNCTION = "function";

	String DEFAULT_ENODE_FUNCTION = "now";

	String ATTR_PROJECT_NAME = "project";

	String ATTR_OTHER_PROJECTS = "otherProjects";

	String ATTR_NODE_NAME = "nodeLabel";

	String ATTR_START_NODE = "startNode";

	// /
	String ATTR_OTP_HOME = "otphome";

	String ERTS_OTP_HOME = prefix + ".otp_home";

	String DEFAULT_OTP_HOME = "";

	String ERTS_PATH_A = prefix + ".path_a";

	String DEFAULT_PATH_A = "";

	String ERTS_PATH_Z = prefix + ".path_z";

	String DEFAULT_PATH_Z = "";

	String ERTS_EXTRA_ARGS = prefix + ".extra_erts_args";

	String DEFAULT_EXTRA_ARGS = " ";

	String ERTS_WORKING_DIR = prefix + ".working_dir";

	String DEFAULT_WORKING_DIR = ".";

}
