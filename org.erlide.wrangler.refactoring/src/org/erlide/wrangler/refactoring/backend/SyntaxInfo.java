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
 * Represents syntax information about a selection
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class SyntaxInfo {
    /**
     * @param type
     *            expression type
     * @param definitionPosLine
     *            expression starting position
     * @param definitionPosColumn
     *            expression starting column
     */
    public SyntaxInfo(final Type type, final int definitionPosLine,
            final int definitionPosColumn) {
        this.definitionPosColumn = definitionPosColumn;
        this.definitionPosLine = definitionPosLine;
        this.type = type;
    }

    protected int definitionPosLine;
    protected int definitionPosColumn;
    protected Type type;

    /**
     * Returns true if the selection is variable
     * 
     * @return boolean
     */
    public boolean isVariable() {
        return type == Type.VARIABLE;
    }

    /**
     * An expression type
     * 
     * @author Gyorgy Orosz
     * @version %I%, %G%
     */
    public enum Type {
        /**
         * selection is an expression
         */
        EXPRESSION,
        /**
         * selection is a variable
         */
        VARIABLE,
        /**
         * selection is sg. else
         */
        NONE;
    }

}
