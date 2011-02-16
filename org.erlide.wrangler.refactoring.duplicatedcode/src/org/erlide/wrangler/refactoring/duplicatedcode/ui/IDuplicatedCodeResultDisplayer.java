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
package org.erlide.wrangler.refactoring.duplicatedcode.ui;

import java.util.List;

import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeElement;

/**
 * Interface for displaying duplicated codeparts.
 * 
 * @author Gyorgy Orosz
 * 
 */
public interface IDuplicatedCodeResultDisplayer {
    /**
     * Show the result of a duplicated search refactoring.
     * 
     * @param result
     *            result of the refactoring - duplicates
     */
    public void showResult(List<DuplicatedCodeElement> result);

}
