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

import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.util.PreferencesUtils;
import org.osgi.service.prefs.Preferences;

public final class RuntimeInfoLoader {
    static final String CODE_PATH = "codePath";
    static final String HOME_DIR = "homeDir";
    static final String ARGS = "args";

    private RuntimeInfoLoader() {
    }

    public static void store(final RuntimeInfo info, final Preferences root) {
        final Preferences node = root.node(info.getName());
        final String code = PreferencesUtils.packList(info.getCodePath());
        node.put(CODE_PATH, code);
        node.put(HOME_DIR, info.getOtpHome());
        node.put(ARGS, info.getArgs());
    }

    public static RuntimeInfo load(final Preferences node) {
        final String path = node.get(CODE_PATH, "");
        final RuntimeInfo info = new RuntimeInfo(node.name(), node.get(HOME_DIR, ""),
                node.get(ARGS, ""), PreferencesUtils.unpackList(path));
        return info;
    }

}
