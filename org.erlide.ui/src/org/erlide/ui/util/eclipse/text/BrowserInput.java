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

/**
 * A browser input contains an input element and a previous and a next input, if
 * available.
 * 
 * The browser input also provides a human readable name of its input element.
 * 
 * @since 3.4
 */
public abstract class BrowserInput {

    private final BrowserInput fPrevious;
    private BrowserInput fNext;

    /**
     * Create a new Browser input.
     * 
     * @param previous
     *            the input previous to this or <code>null</code> if this is the
     *            first
     */
    public BrowserInput(final BrowserInput previous) {
        fPrevious = previous;
        if (previous != null) {
            previous.fNext = this;
        }
    }

    /**
     * The previous input or <code>null</code> if this is the first.
     * 
     * @return the previous input or <code>null</code>
     */
    public BrowserInput getPrevious() {
        return fPrevious;
    }

    /**
     * The next input or <code>null</code> if this is the last.
     * 
     * @return the next input or <code>null</code>
     */
    public BrowserInput getNext() {
        return fNext;
    }

    /**
     * The element to use to set the browsers input.
     * 
     * @return the input element
     */
    public abstract Object getInputElement();

    /**
     * A human readable name for the input.
     * 
     * @return the input name
     */
    public abstract String getInputName();
}
