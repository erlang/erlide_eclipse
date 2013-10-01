/*******************************************************************************
 * Copyright (c) 2004 Lukas Larsson and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Lukas Larsson
 *******************************************************************************/

package org.erlide.ui.wizards.templates;

import java.util.ArrayList;

public class LocalFunctionsVariableResolver extends FunctionVariableResolver {

    private static final ArrayList<FunctionVariableResolver> fInstances = new ArrayList<FunctionVariableResolver>();

    public LocalFunctionsVariableResolver() {
        fInstances.add(this);
    }

    public static LocalFunctionsVariableResolver getDefault() {
        if (fInstances.size() == 0) {
            fInstances.add(new LocalFunctionsVariableResolver());
        }
        return (LocalFunctionsVariableResolver) fInstances.get(0);
    }

    public void addFunction(final String name, final int arity) {
        for (final Object element0 : fInstances) {
            final LocalFunctionsVariableResolver element = (LocalFunctionsVariableResolver) element0;
            element.doAddFunction(name, arity);
        }
    }

    public void clearFunctions() {
        for (final Object element0 : fInstances) {
            final LocalFunctionsVariableResolver element = (LocalFunctionsVariableResolver) element0;
            element.doClearFunctions();
        }
    }

}
