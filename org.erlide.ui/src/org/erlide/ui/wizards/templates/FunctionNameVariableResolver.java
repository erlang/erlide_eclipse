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

import org.eclipse.jface.text.templates.TemplateContext;
import org.eclipse.jface.text.templates.TemplateVariableResolver;

public class FunctionNameVariableResolver extends TemplateVariableResolver {

    private static FunctionNameVariableResolver fInstance;

    private String fFunctionName;

    public static FunctionNameVariableResolver getDefault() {
        if (fInstance == null) {
            fInstance = new FunctionNameVariableResolver();
        }
        return fInstance;
    }

    @Override
    protected String resolve(final TemplateContext context) {
        return getFunctionName();
    }

    /**
     * @return Returns the functionName.
     */
    public String getFunctionName() {
        return fFunctionName;
    }

    /**
     * @param functionName
     *            The functionName to set.
     */
    public void setFunctionName(final String functionName) {
        fFunctionName = functionName;
    }
}
