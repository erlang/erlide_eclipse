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

import org.eclipse.core.resources.IFile;

/**
 * Element, which represents a module in the TreeView
 * 
 * @author Gyorgy Orosz
 * 
 */
public class DuplicatedFileElement extends AbstractResultTreeParent {
    IFile containingFile;

    // ITextEditor editor;

    /**
     * Constructor
     * 
     * @param containFile
     *            file which is represented
     */
    public DuplicatedFileElement(final IFile containFile) {
        // this.editor = editor;
        containingFile = containFile;
    }

    @Override
    public String getName() {
        return containingFile.getName();
    }

    /*
     * public ITextEditor getTextEditor() { return editor; }
     */

    /**
     * Returns the represented file
     * 
     * @return represented file
     */
    public IFile getContainingFile() {
        return containingFile;
    }

}
