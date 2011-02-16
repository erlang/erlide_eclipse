/*******************************************************************************
 * Copyright (c) 2010 György Orosz.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     György Orosz - initial API and implementation
 ******************************************************************************/
package org.erlide.wrangler.refactoring.duplicatedcode.ui.elements;

/**
 * Root element of a TreeView for Duplicates view
 * 
 * @author Gyorgy Orosz
 * 
 */
public class ResultTreeRoot extends AbstractResultTreeParent {

    @Override
    public String getName() {
        return "";
    }

    /**
     * Drops children elements.
     */
    public void dropChildren() {
        if (children != null) {
            children.clear();
        }
    }

}
