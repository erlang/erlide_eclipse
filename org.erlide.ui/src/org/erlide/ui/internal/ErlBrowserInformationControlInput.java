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
package org.erlide.ui.internal;

import org.eclipse.core.runtime.Assert;
import org.erlide.core.model.root.IErlElement;
import org.erlide.ui.util.eclipse.text.BrowserInformationControlInput;

/**
 * Browser input for Edoc hover.
 * 
 * @since 3.4
 */
public class ErlBrowserInformationControlInput extends
        BrowserInformationControlInput {

    private final Object fElement;
    private final String fHtml;
    private final int fLeadingImageWidth;

    /**
     * Creates a new browser information control input.
     * 
     * @param previous
     *            previous input, or <code>null</code> if none available
     * @param element
     *            the element, or <code>null</code> if none available
     * @param html
     *            HTML contents, must not be null
     * @param leadingImageWidth
     *            the indent required for the element image
     */
    public ErlBrowserInformationControlInput(
            final ErlBrowserInformationControlInput previous,
            final Object element, final String html, final int leadingImageWidth) {
        super(previous);
        Assert.isNotNull(html);
        fElement = element;
        fHtml = html;
        fLeadingImageWidth = leadingImageWidth;
    }

    /*
     * @see org.eclipse.jface.internal.text.html.BrowserInformationControlInput#
     * getLeadingImageWidth()
     * 
     * @since 3.4
     */
    @Override
    public int getLeadingImageWidth() {
        return fLeadingImageWidth;
    }

    // /**
    // * Returns the Erlang element.
    // *
    // * @return the element or <code>null</code> if none available
    // */
    // public IErlElement getElement() {
    // return fElement;
    // }

    /*
     * @see org.eclipse.jface.internal.text.html.BrowserInput#getHtml()
     */
    @Override
    public String getHtml() {
        return fHtml;
    }

    /*
     * @see org.eclipse.jdt.internal.ui.infoviews.BrowserInput#getInputElement()
     */
    @Override
    public Object getInputElement() {
        return fElement == null ? fHtml : fElement;
    }

    /*
     * @see org.eclipse.jdt.internal.ui.infoviews.BrowserInput#getInputName()
     */
    @Override
    public String getInputName() {
        if (fElement instanceof IErlElement) {
            final IErlElement element = (IErlElement) fElement;
            return element.getName();
        }
        return ""; //$NON-NLS-1$
    }
}
