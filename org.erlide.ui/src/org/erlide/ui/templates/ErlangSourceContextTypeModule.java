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

import org.eclipse.jface.text.templates.GlobalTemplateVariables;
import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.text.templates.TemplateContextType;
import org.erlide.ui.internal.ErlideUIPlugin;

public class ErlangSourceContextTypeModule extends TemplateContextType {

    private static ErlangSourceContextTypeModule fInstance;

    /** This context's id */
    public static final String ERLANG_SOURCE_CONTEXT_TYPE_MODULE_ID = "org.erlide.ui.erlangsource.template.context.module"; //$NON-NLS-1$

    /**
     * Creates a new XML context type.
     */
    public ErlangSourceContextTypeModule() {
        addGlobalResolvers();
        addModuleResolver();
        fInstance = this;
    }

    private void addModuleResolver() {
        addResolver(new ModuleVariableResolver()); // .getDefault());
    }

    private void addGlobalResolvers() {
        addResolver(new GlobalTemplateVariables.LineSelection());
        addResolver(new GlobalTemplateVariables.Dollar());
        addResolver(new GlobalTemplateVariables.Date());
        addResolver(new GlobalTemplateVariables.Year());
        addResolver(new GlobalTemplateVariables.Time());
        addResolver(new GlobalTemplateVariables.User());
    }

    public void addElementResolvers() {
        final Template[] templates = ErlideUIPlugin
                .getDefault()
                .getTemplateStore()
                .getTemplates(
                        ErlangSourceContextTypeModuleElement.ERLANG_SOURCE_CONTEXT_TYPE_MODULE_ELEMENT_ID);
        for (final Template template : templates) {
            addResolver(new ModuleElementVariableResolver(template.getName(),
                    template));
        }
    }

    public static ErlangSourceContextTypeModule getDefault() {
        if (fInstance == null) {
            new ErlangSourceContextTypeModule();
        }
        return fInstance;
    }

}
