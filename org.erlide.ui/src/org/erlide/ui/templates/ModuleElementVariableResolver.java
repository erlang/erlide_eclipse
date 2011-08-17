/*******************************************************************************
 * Copyright (c) 2004 Lukas Larsson and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Lukas Larsson
 *******************************************************************************/

package org.erlide.ui.templates;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.templates.DocumentTemplateContext;
import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.text.templates.TemplateBuffer;
import org.eclipse.jface.text.templates.TemplateContext;
import org.eclipse.jface.text.templates.TemplateException;
import org.eclipse.jface.text.templates.TemplateVariable;
import org.eclipse.jface.text.templates.TemplateVariableResolver;

public class ModuleElementVariableResolver extends TemplateVariableResolver {

    private final Template fTemplate;
    private boolean reentry = false;

    public ModuleElementVariableResolver(final String type,
            final Template template) {
        super(type, "");
        fTemplate = template;
    }

    @Override
    public void resolve(final TemplateVariable variable,
            final TemplateContext theContext) {
        if (reentry) {
            return;
        }
        reentry = true;
        final DocumentTemplateContext context = new DocumentTemplateContext(
                ErlangSourceContextTypeModule.getDefault(), new Document(
                        fTemplate.getPattern()), 0, fTemplate.getPattern()
                        .length());
        TemplateBuffer tb;
        try {
            tb = context.evaluate(fTemplate);
            variable.setValue(tb.getString());
        } catch (final BadLocationException e) {
        } catch (final TemplateException e) {
        }
        reentry = false;
    }
}
