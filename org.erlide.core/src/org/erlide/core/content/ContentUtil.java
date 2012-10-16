/*******************************************************************************
 * Copyright (c) 2005, 2008 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.core.content;

import java.io.IOException;
import java.io.InputStream;

import org.eclipse.core.runtime.content.IContentDescription;

public class ContentUtil {
    /*
     * Reads bom from the stream. Note that the stream will not be repositioned
     * when the method returns.
     */
    public static byte[] getByteOrderMark(final InputStream input)
            throws IOException {
        final int first = input.read();
        if (first == 0xEF) {
            // look for the UTF-8 Byte Order Mark (BOM)
            final int second = input.read();
            final int third = input.read();
            if (second == 0xBB && third == 0xBF) {
                return IContentDescription.BOM_UTF_8;
            }
        } else if (first == 0xFE) {
            // look for the UTF-16 BOM
            if (input.read() == 0xFF) {
                return IContentDescription.BOM_UTF_16BE;
            }
        } else if (first == 0xFF) {
            if (input.read() == 0xFE) {
                return IContentDescription.BOM_UTF_16LE;
            }
        }
        return null;
    }
}
