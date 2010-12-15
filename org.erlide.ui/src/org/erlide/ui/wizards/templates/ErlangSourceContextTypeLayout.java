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

import org.eclipse.jface.text.templates.GlobalTemplateVariables;
import org.eclipse.jface.text.templates.TemplateContextType;

public class ErlangSourceContextTypeLayout extends TemplateContextType {

    private static ErlangSourceContextTypeLayout fInstance;

    /**
     * Creates a new XML context type.
     */
    public ErlangSourceContextTypeLayout() {
        addGlobalResolvers();
        fInstance = this;
    }

    private void addGlobalResolvers() {
        addResolver(new GlobalTemplateVariables.Cursor());
        addResolver(new GlobalTemplateVariables.WordSelection());
        addResolver(new GlobalTemplateVariables.LineSelection());
        addResolver(new GlobalTemplateVariables.Dollar());
        addResolver(new GlobalTemplateVariables.Date());
        addResolver(new GlobalTemplateVariables.Year());
        addResolver(new GlobalTemplateVariables.Time());
        addResolver(new GlobalTemplateVariables.User());
    }

    public static ErlangSourceContextTypeLayout getDefault() {
        if (fInstance == null) {
            new ErlangSourceContextTypeLayout();
        }
        return fInstance;
    }

}
