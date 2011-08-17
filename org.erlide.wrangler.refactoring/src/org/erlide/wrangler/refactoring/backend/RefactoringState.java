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
package org.erlide.wrangler.refactoring.backend;

/**
 * State of a Wrangler Refactoring.
 * 
 * The class does not intend to cover all the cases, just thise which are
 * essential.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public enum RefactoringState {
    /**
     * refactoring succeeds
     */
    OK,
    /**
     * refactoring fails
     */
    ERROR,
    /**
     * refactoring succeeds, but has warning message
     */
    WARNING,
    /**
     * refactoring returns with a question
     */
    QUESTION,
    /**
     * multi_instances - for fold expression
     */
    MULTI_INSTANCES,
    /**
     * unknown_side_effect - for fold expression
     */
    UNKNOWN_SIDE_EFFECT,
    /**
     * more_than_one_clause - for fold expression
     */
    MORE_THAN_ONE_CLAUSE;
}
