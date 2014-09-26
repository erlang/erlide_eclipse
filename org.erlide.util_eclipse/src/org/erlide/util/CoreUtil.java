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
package org.erlide.util;

import org.eclipse.core.runtime.IPath;

public final class CoreUtil {

    /*
     * Returns the index of the most specific argument paths which is strictly
     * enclosing the path to check
     */
    public static int indexOfEnclosingPath(final IPath checkedPath, final IPath[] paths,
            final int pathCount) {

        int bestMatch = -1, bestLength = -1;
        for (int i = 0; i < pathCount; i++) {
            if (paths[i].equals(checkedPath)) {
                continue;
            }
            if (paths[i].isPrefixOf(checkedPath)) {
                final int currentLength = paths[i].segmentCount();
                if (currentLength > bestLength) {
                    bestLength = currentLength;
                    bestMatch = i;
                }
            }
        }
        return bestMatch;
    }

    /*
     * Returns the index of the first argument paths which is equal to the path
     * to check
     */
    public static int indexOfMatchingPath(final IPath checkedPath, final IPath[] paths,
            final int pathCount) {

        for (int i = 0; i < pathCount; i++) {
            if (paths[i].equals(checkedPath)) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Returns the index of the first argument paths which is strictly nested
     * inside the path to check
     */
    public static int indexOfNestedPath(final IPath checkedPath, final IPath[] paths,
            final int pathCount) {

        for (int i = 0; i < pathCount; i++) {
            if (checkedPath.equals(paths[i])) {
                continue;
            }
            if (checkedPath.isPrefixOf(paths[i])) {
                return i;
            }
        }
        return -1;
    }

    private CoreUtil() {
    }

}
