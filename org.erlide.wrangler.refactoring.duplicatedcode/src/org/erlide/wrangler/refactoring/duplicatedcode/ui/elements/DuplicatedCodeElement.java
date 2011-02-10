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
 * TreeView element, which represents a set of duplicates
 * 
 * @author Gyorgy Orosz
 * 
 */
public class DuplicatedCodeElement extends AbstractResultTreeParent {

    String codePartId;
    DuplicatedCodeInstanceElement defaultCodePart;

    /**
     * Constructor
     * 
     * @param defaultInstance
     *            a particular element, which will be used to show the
     *            corresponding code part
     */
    public DuplicatedCodeElement(
            final DuplicatedCodeInstanceElement defaultInstance) {
        defaultCodePart = defaultInstance;
    }

    @Override
    public String getName() {
        return "\"" + defaultCodePart.getCodePartString() + "\"";
    }

    /**
     * Returns the default code part
     * 
     * @return default code part
     */
    public DuplicatedCodeInstanceElement getCodePart() {
        return defaultCodePart;
    }
}
