/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.internal.util;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;

public class SelectionUtil {

    // public static List toList(ISelection selection) {
    // if (selection instanceof IStructuredSelection) {
    // return ((IStructuredSelection) selection).toList();
    // }
    // return null;
    // }

    /**
     * Returns the selected element if the selection consists of a single
     * element only.
     * 
     * @param selection
     *            the selection
     * @return the selected first element or null
     * 
     */
    public static Object getSingleElement(final ISelection s) {
        if (!(s instanceof IStructuredSelection)) {
            return null;
        }
        final IStructuredSelection selection = (IStructuredSelection) s;
        if (selection.size() != 1) {
            return null;
        }

        return selection.getFirstElement();
    }
}
