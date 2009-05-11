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
package org.erlide.runtime;

import org.erlide.core.ErlangPlugin;

public interface ErlLaunchAttributes {

	final String prefix = ErlangPlugin.PLUGIN_ID;

	final String PROJECTS = prefix + ".projects";

	final String MODULE = prefix + ".module";
	final String FUNCTION = prefix + ".function";
	final String ARGUMENTS = prefix + ".arguments";

	final String RUNTIME_NAME = prefix + ".runtime";
	final String DEFAULT_RUNTIME_NAME = "erts";

	final String NODE_NAME = prefix + ".nodeName";
	final String COOKIE = prefix + ".cookie";
	final String START_ME = prefix + ".startMe";

	final String DEBUG_FLAGS = prefix + ".debugFlags"; // @see
	// IErlDebugConstants

	final String DEBUG_INTERPRET_MODULES = prefix + ".interpretModules";
}
