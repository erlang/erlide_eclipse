/*******************************************************************************
 * Copyright (c) 2010-2010 Jakob and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Jakob
 *******************************************************************************/

package org.erlide.ui.templates;

import org.eclipse.jface.text.templates.GlobalTemplateVariables;
import org.eclipse.jface.text.templates.TemplateContextType;

public class ErlangSourceContextTypeModuleElement extends TemplateContextType {

    private static ErlangSourceContextTypeModuleElement fInstance;

    /** This context's id */
    public static final String ERLANG_SOURCE_CONTEXT_TYPE_MODULE_ELEMENT_ID = "org.erlide.ui.erlangsource.template.context.module.element"; //$NON-NLS-1$

    public static ErlangSourceContextTypeModuleElement getDefault() {
        if (fInstance == null) {
            new ErlangSourceContextTypeModuleElement();
        }
        return fInstance;
    }

    /**
     * Creates a new XML context type.
     */
    public ErlangSourceContextTypeModuleElement() {
        addGlobalResolvers();
        fInstance = this;
    }

    private void addGlobalResolvers() {
        addResolver(new GlobalTemplateVariables.LineSelection());
        addResolver(new GlobalTemplateVariables.Dollar());
        addResolver(new GlobalTemplateVariables.Date());
        addResolver(new GlobalTemplateVariables.Year());
        addResolver(new GlobalTemplateVariables.Time());
        addResolver(new GlobalTemplateVariables.User());
    }

    // public static ErlangSourceContextTypeModule getDefault() {
    // if (fInstance == null) {
    // fInstance = new ErlangSourceContextTypeModule();
    // }
    // return fInstance;
    // }

}
