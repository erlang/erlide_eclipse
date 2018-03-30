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
package org.erlide.wrangler.refactoring.ui.validator;

/**
 * Validate a string which is an Erlang atom.ss
 *
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class AtomValidator implements IValidator {

    @Override
    public boolean isValid(final String s) {
        if (s.isEmpty()) {
            return false;
        }
        if (s.charAt(0) == '\'' && s.charAt(s.length() - 1) == '\'') {
            return true;
        }
        if (s.substring(0, 1).replaceAll("[a-z]", "").isEmpty()
                && s.replaceAll("[A-Za-z_@0-9]", "").isEmpty()) {
            return true;
        }
        return false;
    }

}
