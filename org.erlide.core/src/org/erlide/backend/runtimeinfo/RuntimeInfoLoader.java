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
package org.erlide.backend.runtimeinfo;

import org.erlide.utils.PreferencesUtils;
import org.osgi.service.prefs.Preferences;

public class RuntimeInfoLoader {
    static final String CODE_PATH = "codePath";
    static final String HOME_DIR = "homeDir";
    static final String ARGS = "args";

    private final RuntimeInfo info;

    public RuntimeInfoLoader(final RuntimeInfo info) {
        this.info = info;
    }

    public void store(final Preferences root) {
        final Preferences node = root.node(info.getName());
        final String code = PreferencesUtils.packList(info.getCodePath());
        node.put(CODE_PATH, code);
        node.put(HOME_DIR, info.getOtpHome());
        node.put(ARGS, info.getArgs());
    }

    public void load(final Preferences node) {
        info.setName(node.name());
        final String path = node.get(CODE_PATH, "");
        info.setCodePath(PreferencesUtils.unpackList(path));
        info.setOtpHome(node.get(HOME_DIR, ""));
        info.setArgs(node.get(ARGS, ""));
    }

}
