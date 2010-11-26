/*******************************************************************************
 * Copyright (c) 2008 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.util.eclipse.text;

import org.eclipse.jface.text.DefaultInformationControl;

/**
 * Provides input for a {@link BrowserInformationControl}.
 * 
 * @since 3.4
 */
public abstract class BrowserInformationControlInput extends BrowserInput {

    /**
     * Returns the leading image width.
     * 
     * @return the size of the leading image, by default <code>0</code> is
     *         returned
     * @since 3.4
     */
    public int getLeadingImageWidth() {
        return 0;
    }

    /**
     * Creates the next browser input with the given input as previous one.
     * 
     * @param previous
     *            the previous input or <code>null</code> if none
     */
    public BrowserInformationControlInput(
            final BrowserInformationControlInput previous) {
        super(previous);
    }

    /**
     * @return the HTML contents
     */
    public abstract String getHtml();

    /**
     * Returns the HTML from {@link #getHtml()}. This is a fallback mode for
     * platforms where the {@link BrowserInformationControl} is not available
     * and this input is passed to a {@link DefaultInformationControl}.
     * 
     * @return {@link #getHtml()}
     */
    @Override
    public String toString() {
        return getHtml();
    }
}
