/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Vlad Dumitrescu - Erlang adaptation
 *******************************************************************************/
package org.erlide.core.services.codeassist;

import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlElement;

/**
 * Common protocol for Erlang elements that support source code assist and code
 * resolve.
 * 
 * @noimplement This interface is not intended to be implemented by clients.
 */
public interface ICodeAssist {

    /**
     * Performs code completion at the given offset position in this compilation
     * unit, reporting results to the given completion requestor. The
     * <code>offset</code> is the 0-based index of the character, after which
     * code assist is desired. An <code>offset</code> of -1 indicates to code
     * assist at the beginning of this compilation unit.
     * <p>
     * 
     * @param offset
     *            the given offset position
     * @param requestor
     *            the given completion requestor
     * @exception ErlModelException
     *                if code assist could not be performed. Reasons include:
     *                <ul>
     *                <li>This Erlang element does not exist
     *                (ELEMENT_DOES_NOT_EXIST)</li> <li> The position specified
     *                is < -1 or is greater than this compilation unit's source
     *                length (INDEX_OUT_OF_BOUNDS)
     *                </ul>
     * 
     * @exception IllegalArgumentException
     *                if <code>requestor</code> is <code>null</code>
     */

    String[] codeComplete(int offset) throws ErlModelException;

    /**
     * Returns the Erlang elements corresponding to the given selected text in
     * this compilation unit. The <code>offset</code> is the 0-based index of
     * the first selected character. The <code>length</code> is the number of
     * selected characters.
     * 
     * @param offset
     *            the given offset position
     * @param length
     *            the number of selected characters
     * @return the Erlang elements correspondiing to the given selected text
     * 
     * @exception ErlModelException
     *                if code resolve could not be performed. Reasons include:
     *                <li>This Erlang element does not exist
     *                (ELEMENT_DOES_NOT_EXIST)</li> <li>The range specified is
     *                not within this element's source range
     *                (INDEX_OUT_OF_BOUNDS) </ul>
     * 
     */
    IErlElement[] codeSelect(int offset, int length) throws ErlModelException;
}
