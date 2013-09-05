/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *     Jakob C
 *******************************************************************************/
package org.erlide.engine.internal.util;

public final class ErlideUtil {

    private static Boolean fgCacheNoModelCache = null;

    public static boolean isCacheDisabled() {
        if (fgCacheNoModelCache == null) {
            final String test = System.getProperty("erlide.noModelCache");
            fgCacheNoModelCache = Boolean.valueOf(test);
        }
        return fgCacheNoModelCache.booleanValue();
    }

    private ErlideUtil() {
    }
}
