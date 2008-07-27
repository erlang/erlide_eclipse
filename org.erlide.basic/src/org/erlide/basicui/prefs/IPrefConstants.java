/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *     Vlad Dumitrescu
 *     Mickaël Rémond
 *******************************************************************************/
package org.erlide.basicui.prefs;

/**
 * Constants used in project and plugin preferences
 * 
 * @author Eric Merritt [cyberlync at yahoo dot com]
 */
public interface IPrefConstants {

	static final String ERTS_HOME_DIR = "otp_home";

	static final String DEFAULT_OTP_HOME = "";

	static final String ERTS_CODE_PATH = "code_path";

	static final String DEFAULT_CODE_PATH = "*PROJECT*";

	static final String ERTS_DEFAULT_ARGS = "default_erts_args";

	static final String DEFAULT_EXTRA_ARGS = "";
}
