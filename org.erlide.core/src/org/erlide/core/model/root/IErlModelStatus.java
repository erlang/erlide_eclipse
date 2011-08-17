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
package org.erlide.core.model.root;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;

/**
 * Represents the outcome of an Erlang model operation. Status objects are used
 * inside <code>ErlModelException</code> objects to indicate what went wrong.
 * <p>
 * Erlang model status object are distinguished by their plug-in id:
 * <code>getPlugin</code> returns <code>"org.erlide.core.model.erlang"</code>.
 * <code>getCode</code> returns one of the status codes declared in
 * <code>IErlModelStatusConstants</code>.
 * </p>
 * <p>
 * A Erlang model status may also carry additional information (that is, in
 * addition to the information defined in <code>IStatus</code>):
 * <ul>
 * <li>elements - optional handles to Erlang elements associated with the
 * failure</li>
 * <li>string - optional string associated with the failure</li>
 * </ul>
 * <p>
 * This interface is not intended to be implemented by clients.
 * </p>
 * 
 * @see org.eclipse.core.runtime.IStatus
 * @see ErlModelStatusConstants
 */
public interface IErlModelStatus extends IStatus {

    /**
     * Returns any Erlang elements associated with the failure (see
     * specification of the status code), or an empty array if no elements are
     * related to this particular status code.
     * 
     * @return the list of Erlang element culprits
     * @see ErlModelStatusConstants
     */
    IErlElement[] getElements();

    /**
     * Returns the path associated with the failure (see specification of the
     * status code), or <code>null</code> if the failure is not one of
     * <code>DEVICE_PATH</code>, <code>INVALID_PATH</code>,
     * <code>PATH_OUTSIDE_PROJECT</code>, or <code>RELATIVE_PATH</code>.
     * 
     * @return the path that caused the failure, or <code>null</code> if none
     * @see ErlModelStatusConstants#DEVICE_PATH
     * @see ErlModelStatusConstants#INVALID_PATH
     * @see ErlModelStatusConstants#PATH_OUTSIDE_PROJECT
     * @see ErlModelStatusConstants#RELATIVE_PATH
     */
    IPath getPath();

    /**
     * Returns the string associated with the failure (see specification of the
     * status code), or <code>null</code> if no string is related to this
     * particular status code.
     * 
     * @return the string culprit, or <code>null</code> if none
     * @see ErlModelStatusConstants
     * @deprecated Use IStatus#getMessage instead
     */
    @Deprecated
    String getString();

    /**
     * Returns whether this status indicates that a Erlang model element does
     * not exist. This convenience method is equivalent to
     * <code>getCode() == IErlModelStatusConstants.ELEMENT_DOES_NOT_EXIST</code>
     * .
     * 
     * @return <code>true</code> if the status code indicates that a Erlang
     *         model element does not exist
     * @see ErlModelStatusConstants#ELEMENT_DOES_NOT_EXIST
     */
    boolean isDoesNotExist();
}
