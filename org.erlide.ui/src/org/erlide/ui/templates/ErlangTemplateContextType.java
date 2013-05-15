package org.erlide.ui.templates;

import org.eclipse.jface.text.templates.GlobalTemplateVariables;
import org.eclipse.jface.text.templates.TemplateContextType;

public class ErlangTemplateContextType extends TemplateContextType {

    /** This context's id */
    public static final String ERLANG_CONTEXT_TYPE_ID = "org.erlide.ui.editor.erlang"; //$NON-NLS-1$

    /**
     * Creates a new XML context type.
     */
    public ErlangTemplateContextType() {
        addGlobalResolvers();
        addModuleResolver();
    }

    private void addModuleResolver() {
        addResolver(new ModuleVariableResolver());
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

}
