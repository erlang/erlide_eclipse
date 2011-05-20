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
import java.util.Iterator;

import org.eclipse.jface.text.templates.TemplateContext;
import org.eclipse.jface.text.templates.TemplateVariable;

public class ExportsVariableResolver extends FunctionVariableResolver {

    private static final ArrayList<FunctionVariableResolver> fInstances = new ArrayList<FunctionVariableResolver>();

    public ExportsVariableResolver() {
        fInstances.add(this);
    }

    public static ExportsVariableResolver getDefault() {
        if (fInstances.size() == 0) {
            fInstances.add(new ExportsVariableResolver());
        }
        return (ExportsVariableResolver) fInstances.get(0);
    }

    public void addFunction(final String name, final int arity) {
        for (final Object element0 : fInstances) {
            final ExportsVariableResolver element = (ExportsVariableResolver) element0;
            element.doAddFunction(name, arity);
        }
    }

    public void clearFunctions() {
        for (final Object element0 : fInstances) {
            final ExportsVariableResolver element = (ExportsVariableResolver) element0;
            element.doClearFunctions();
        }
    }

    @Override
    public void resolve(final TemplateVariable variable,
            final TemplateContext context) {
        final StringBuilder buff = new StringBuilder();

        for (final Iterator<Object[]> iter = functions.iterator(); iter
                .hasNext();) {
            final String[] part = buff.toString().split("\\n");
            if (part[part.length - 1].length() > 60) {
                buff.append('\n');
            }
            final Object[] element = iter.next();
            buff.append(element[0] + "/" + element[1]);
            if (iter.hasNext()) {
                buff.append(", ");
            }
        }

        variable.setValue(buff.toString());
    }

}
