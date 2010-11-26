/*******************************************************************************
 * Copyright (c) 2004 Lukas Larsson and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Lukas Larsson
 *******************************************************************************/

package org.erlide.ui.wizards;

public class Function {

    public int arity;

    public String name;

    public boolean isState;

    public boolean isExported;

    @Override
    public String toString() {
        return "<Function name=" + name + " arity=" + arity + " isExported="
                + isState + " isState=" + isState + ">";
    }
}
