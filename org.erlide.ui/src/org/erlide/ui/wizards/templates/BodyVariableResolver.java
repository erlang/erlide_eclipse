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

public class BodyVariableResolver extends TemplateVariableResolver {

    private static BodyVariableResolver fInstance;

    private String fBody = "ok";

    public static BodyVariableResolver getDefault() {
        if (fInstance == null) {
            fInstance = new BodyVariableResolver();
        }
        return fInstance;
    }

    @Override
    protected String resolve(final TemplateContext context) {
        return fBody;
    }

    /**
     * @return Returns the body.
     */
    public String getBody() {
        return fBody;
    }

    /**
     * @param body
     *            The body to set.
     */
    public void setBody(final String body) {
        fBody = body;
    }
}
