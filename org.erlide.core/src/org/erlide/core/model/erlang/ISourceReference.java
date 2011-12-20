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
package org.erlide.core.model.erlang;

import org.erlide.core.model.root.ErlModelException;

/**
 * Common protocol for Erlang elements that have associated source code. This
 * set consists of <code>IErlModule</code>, <code>IErlAttribute</code>,
 * <code>IErlFunction</code>, <code>IErlFunctionClause</code> and
 * <code>IErlError</code>. </ul>
 * <p>
 * Source reference elements may be working copies if they were created from a
 * compilation unit that is a working copy.
 * </p>
 * <p>
 * This interface is not intended to be implemented by clients.
 * </p>
 * 
 */
public interface ISourceReference {

    /**
     * Returns the source code associated with this element. This extracts the
     * substring from the source buffer containing this source element. This
     * corresponds to the source range that would be returned by
     * <code>getSourceRange</code>.
     * <p>
     * For beam files, this returns the source of the entire compilation unit
     * associated with the beam file (if there is one).
     * </p>
     * 
     * @return the source code, or <code>null</code> if this element has no
     *         associated source code
     * @exception ErlModelException
     *                if an exception occurs while accessing its corresponding
     *                resource
     */
    String getSource() throws ErlModelException;

    /**
     * Returns the source range associated with this element.
     * <p>
     * For beam files, this returns the range of the entire compilation unit
     * associated with the beam file (if there is one).
     * </p>
     * 
     * @return the source range, or <code>null</code> if this element has no
     *         associated source code
     */
    ISourceRange getSourceRange();

    public int getLineStart();

    public int getLineEnd();

}
