/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.util.eclipse.text;

import java.io.IOException;
import java.io.Reader;

/**
 * <p>
 * Moved into this package from
 * <code>org.eclipse.jface.internal.text.revisions</code>.
 * </p>
 */
public abstract class SingleCharReader extends Reader {

    /**
     * @see Reader#read()
     */
    @Override
    public abstract int read() throws IOException;

    /**
     * @see Reader#read(char[],int,int)
     */
    @Override
    public int read(final char cbuf[], final int off, final int len)
            throws IOException {
        final int end = off + len;
        for (int i = off; i < end; i++) {
            final int ch = read();
            if (ch == -1) {
                if (i == off) {
                    return -1;
                }
                return i - off;
            }
            cbuf[i] = (char) ch;
        }
        return len;
    }

    /**
     * @see Reader#ready()
     */
    @Override
    public boolean ready() throws IOException {
        return true;
    }

    /**
     * Returns the readable content as string.
     * 
     * @return the readable content as string
     * @exception IOException
     *                in case reading fails
     */
    public String getString() throws IOException {
        final StringBuffer buf = new StringBuffer();
        int ch;
        while ((ch = read()) != -1) {
            buf.append((char) ch);
        }
        return buf.toString();
    }
}
