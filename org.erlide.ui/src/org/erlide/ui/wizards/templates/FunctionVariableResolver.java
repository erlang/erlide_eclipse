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

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.templates.DocumentTemplateContext;
import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.text.templates.TemplateBuffer;
import org.eclipse.jface.text.templates.TemplateContext;
import org.eclipse.jface.text.templates.TemplateException;
import org.eclipse.jface.text.templates.TemplateVariable;
import org.eclipse.jface.text.templates.TemplateVariableResolver;
import org.erlide.jinterface.ErlLogger;

public class FunctionVariableResolver extends TemplateVariableResolver {

    protected ArrayList<Object[]> functions = new ArrayList<Object[]>();

    @Override
    public void resolve(final TemplateVariable variable,
            final TemplateContext context) {
        @SuppressWarnings("unchecked")
        final Iterator<TemplateVariableResolver> it = ErlangSourceContextTypeLayout
                .getDefault().resolvers();
        FunctionNameVariableResolver name_var = null;
        // !TODO: Use BodyVariableResolver
        // BodyVariableResolver body_var = null;
        ArgumentsVariableResolver arg_var = null;
        while (it.hasNext()) {
            final TemplateVariableResolver element = it.next();
            if (element instanceof FunctionNameVariableResolver) {
                name_var = (FunctionNameVariableResolver) element;
            } else if (element instanceof BodyVariableResolver) {
                // body_var = (BodyVariableResolver) element;
            } else if (element instanceof ArgumentsVariableResolver) {
                arg_var = (ArgumentsVariableResolver) element;
            }
        }

        if (arg_var == null || name_var == null
        // || body_var == null
        ) {
            variable.setValue("");
            return;
        }

        final StringBuilder buff = new StringBuilder();

        for (final Object[] element : functions) {
            arg_var.setArity(((Integer) element[1]).intValue());
            name_var.setFunctionName((String) element[0]);

            final Template commentTemplate = ErlangSourceContextTypeComment
                    .getDefault()
                    .getTemplateStore()
                    .getTemplateData(
                            "org.erlide.ui.erlangsource.functioncomment")
                    .getTemplate();

            DocumentTemplateContext commentContext = new DocumentTemplateContext(
                    ErlangSourceContextTypeLayout.getDefault(), new Document(
                            commentTemplate.getPattern()), 0, commentTemplate
                            .getPattern().length());
            TemplateBuffer tb = null;
            try {
                tb = commentContext.evaluate(commentTemplate);
            } catch (final BadLocationException e) {
                ErlLogger.warn(e);
                buff.append("Error: " + e.getMessage());
            } catch (final TemplateException e) {
                ErlLogger.warn(e);
                buff.append("Error: " + commentTemplate.getName()
                        + " could not be validated!");
            }

            if (tb != null) {
                buff.append(tb.getString() + "\n");
            }

            final Template template = ErlangSourceContextTypeComment
                    .getDefault()
                    .getTemplateStore()
                    .getTemplateData(
                            "org.erlide.ui.erlangsource.functionlayout")
                    .getTemplate();

            commentContext = new DocumentTemplateContext(
                    ErlangSourceContextTypeLayout.getDefault(), new Document(
                            template.getPattern()), 0, template.getPattern()
                            .length());
            try {
                tb = commentContext.evaluate(template);
            } catch (final BadLocationException e) {
                ErlLogger.warn(e);
                buff.append("Error: " + e.getMessage());
            } catch (final TemplateException e) {
                ErlLogger.warn(e);
                buff.append("Error: " + template.getName()
                        + " could not be validated!");
            }

            if (tb != null) {
                buff.append(tb.getString() + "\n");
            }

        }

        variable.setValue(buff.toString());
    }

    public void doAddFunction(final String name, final int arity) {
        final Object[] data = { name, Integer.valueOf(arity) };
        functions.add(data);
    }

    public void doClearFunctions() {
        functions.clear();
    }
}
