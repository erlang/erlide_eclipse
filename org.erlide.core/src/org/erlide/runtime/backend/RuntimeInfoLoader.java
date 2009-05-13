/*******************************************************************************
 * Copyright (c) 2009 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.backend;

import org.erlide.jinterface.backend.RuntimeInfo;
import org.erlide.jinterface.backend.util.PreferencesUtils;
import org.osgi.service.prefs.Preferences;

public class RuntimeInfoLoader {
	static final String CODE_PATH = "codePath";
	static final String HOME_DIR = "homeDir";
	static final String ARGS = "args";
	static final String WORKING_DIR = "workingDir";
	static final String MANAGED = "managed";

	private RuntimeInfo info;

	public RuntimeInfoLoader(RuntimeInfo info) {
		this.info = info;
	}

	public void store(final Preferences root) {
		final Preferences node = root.node(info.getName());
		final String code = PreferencesUtils.packList(info.getCodePath());
		node.put(CODE_PATH, code);
		node.put(HOME_DIR, info.getOtpHome());
		node.put(ARGS, info.getArgs());
		node.put(WORKING_DIR, info.getWorkingDir());
		node.putBoolean(MANAGED, info.isManaged());
	}

	public void load(final Preferences node) {
		info.setName(node.name());
		final String path = node.get(CODE_PATH, "");
		info.setCodePath(PreferencesUtils.unpackList(path));
		info.setOtpHome(node.get(HOME_DIR, ""));
		info.setArgs(node.get(ARGS, ""));
		final String wd = node.get(WORKING_DIR, info.getWorkingDir());
		if (wd.length() != 0) {
			info.setWorkingDir(wd);
		}
		info.setManaged(node.getBoolean(MANAGED, true));
	}

}
