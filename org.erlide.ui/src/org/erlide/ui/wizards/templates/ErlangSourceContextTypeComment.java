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

import java.io.IOException;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.text.templates.ContextTypeRegistry;
import org.eclipse.jface.text.templates.GlobalTemplateVariables;
import org.eclipse.jface.text.templates.TemplateContextType;
import org.eclipse.jface.text.templates.persistence.TemplateStore;
import org.eclipse.ui.editors.text.templates.ContributionContextTypeRegistry;
import org.eclipse.ui.editors.text.templates.ContributionTemplateStore;
import org.erlide.ui.internal.ErlideUIPlugin;

public class ErlangSourceContextTypeComment extends TemplateContextType {

    /** Key to store custom templates. */
    private static final String ERLANGSOURCE_TEMPLATES_KEY = "org.erlide.ui.erlangsource.template"; //$NON-NLS-1$

    /** This context's ids */
    public static final String ERLANGSOURCE_CONTEXT_TYPE_COMMENTS = "org.erlide.ui.erlangsource.template.context.generate"; //$NON-NLS-1$

    public static final String ERLANGSOURCE_CONTEXT_TYPE_BEHAVIOUR = "org.erlide.ui.erlangsource.template.context.behaviour";

    public static final String ERLANGSOURCE_CONTEXT_TYPE_LAYOUT = "org.erlide.ui.erlangsource.template.context.layout";

    /** The shared instance. */
    private static ErlangSourceContextTypeComment fInstance;

    /** The template store. */
    private TemplateStore fStore;

    /** The context type registry. */
    private ContributionContextTypeRegistry fRegistry;

    /**
     * Creates a new XML context type.
     */
    public ErlangSourceContextTypeComment() {
        addGlobalResolvers();
        fInstance = this;
    }

    private void addGlobalResolvers() {
        addResolver(new GlobalTemplateVariables.Dollar());
        addResolver(new GlobalTemplateVariables.Date());
        addResolver(new GlobalTemplateVariables.Year());
        addResolver(new GlobalTemplateVariables.Time());
        addResolver(new GlobalTemplateVariables.User());
    }

    /**
     * Returns the shared instance.
     * 
     * @return the shared instance
     */
    public static ErlangSourceContextTypeComment getDefault() {
        if (fInstance == null) {
            new ErlangSourceContextTypeComment();
        }
        return fInstance;
    }

    /**
     * Returns this plug-in's template store.
     * 
     * @return the template store of this plug-in instance
     */
    public TemplateStore getTemplateStore() {
        if (fStore == null) {
            fStore = new ContributionTemplateStore(getContextTypeRegistry(),
                    ErlideUIPlugin.getDefault().getPreferenceStore(),
                    ERLANGSOURCE_TEMPLATES_KEY);
            try {
                fStore.load();
            } catch (final IOException e) {
                ErlideUIPlugin
                        .getDefault()
                        .getLog()
                        .log(new Status(
                                IStatus.ERROR,
                                "org.erlide.ui.erlangsource.template", IStatus.OK, "", e)); //$NON-NLS-1$ //$NON-NLS-2$
            }
        }
        return fStore;
    }

    /**
     * Returns this plug-in's context type registry.
     * 
     * @return the context type registry for this plug-in instance
     */
    public ContextTypeRegistry getContextTypeRegistry() {
        if (fRegistry == null) {
            // create an configure the contexts available in the template editor
            fRegistry = new ContributionContextTypeRegistry();
            fRegistry.addContextType(ERLANGSOURCE_CONTEXT_TYPE_COMMENTS);
            fRegistry.addContextType(ERLANGSOURCE_CONTEXT_TYPE_BEHAVIOUR);
            fRegistry.addContextType(ERLANGSOURCE_CONTEXT_TYPE_LAYOUT);
        }
        return fRegistry;
    }
}
