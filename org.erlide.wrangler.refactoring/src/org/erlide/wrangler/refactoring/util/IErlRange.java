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
package org.erlide.wrangler.refactoring.util;

/**
 * Interface for defining range in Text documents
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public interface IErlRange extends IRange {

    /**
     * Get the selection's offset
     * 
     * @return offset of the selection
     */
    public int getOffset();

    /**
     * Get the selection's length
     * 
     * @return length of the selection
     */
    public int getLength();
}
