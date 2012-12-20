/*******************************************************************************
 * Copyright (c) 2009 * and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     *
 *******************************************************************************/
package org.erlide.utils;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.content.IContentType;
import org.eclipse.core.runtime.content.IContentTypeManager;

public class CommonUtils {

    private CommonUtils() {
    }

    public static boolean isErlangFileContentFileName(final String fileName) {
        final IContentTypeManager contentTypeManager = Platform
                .getContentTypeManager();
        final IContentType[] contentTypes = contentTypeManager
                .findContentTypesFor(fileName);
        for (final IContentType contentType : contentTypes) {
            if (contentType.getId().equals("org.erlide.core.content.erlang")) {
                return true;
            }
        }
        return false;
    }

}
