/*******************************************************************************
 * Copyright (c) 2004 IBM and others.
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

public class ArgumentsVariableResolver extends TemplateVariableResolver {

    private static ArgumentsVariableResolver fInstance;

    private int fArity;

    public static ArgumentsVariableResolver getDefault() {
        if (fInstance == null) {
            fInstance = new ArgumentsVariableResolver();
        }
        return fInstance;
    }

    @Override
    protected String resolve(final TemplateContext context) {
        final StringBuilder buff = new StringBuilder();
        buff.append(fArity != 0 ? "_Arg0" : "");
        for (int i = 1; i < fArity; i++) {
            buff.append(", _Arg" + i);
        }
        return buff.toString();
    }

    /**
     * @return Returns the arity.
     */
    public int getArity() {
        return fArity;
    }

    /**
     * @param arity
     *            The arity to set.
     */
    public void setArity(final int arity) {
        fArity = arity;
    }
}
