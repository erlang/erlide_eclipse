/*******************************************************************************
 * Copyright (c) 2004 Lukas Larsson and others. All rights reserved. This program and the
 * accompanying materials are made available under the terms of the Eclipse Public License
 * v1.0 which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: Lukas Larsson
 *******************************************************************************/

package org.erlide.ui.wizards.templates;

import java.util.ArrayList;
import java.util.List;

public class LocalFunctionsVariableResolver extends FunctionVariableResolver {

    private static final List<FunctionVariableResolver> fInstances = new ArrayList<>();

    public LocalFunctionsVariableResolver() {
        LocalFunctionsVariableResolver.fInstances.add(this);
    }

    public static LocalFunctionsVariableResolver getDefault() {
        if (LocalFunctionsVariableResolver.fInstances.isEmpty()) {
            LocalFunctionsVariableResolver.fInstances
                    .add(new LocalFunctionsVariableResolver());
        }
        return (LocalFunctionsVariableResolver) LocalFunctionsVariableResolver.fInstances
                .get(0);
    }

    public void addFunction(final String name, final int arity) {
        for (final Object element0 : LocalFunctionsVariableResolver.fInstances) {
            final LocalFunctionsVariableResolver element = (LocalFunctionsVariableResolver) element0;
            element.doAddFunction(name, arity);
        }
    }

    public void clearFunctions() {
        for (final Object element0 : LocalFunctionsVariableResolver.fInstances) {
            final LocalFunctionsVariableResolver element = (LocalFunctionsVariableResolver) element0;
            element.doClearFunctions();
        }
    }

}
