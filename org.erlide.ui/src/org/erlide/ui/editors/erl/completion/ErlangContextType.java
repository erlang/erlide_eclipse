package org.erlide.ui.editors.erl.completion;

import org.eclipse.jface.text.templates.GlobalTemplateVariables;
import org.eclipse.jface.text.templates.TemplateContextType;

public class ErlangContextType extends TemplateContextType {

	/** This context's id */
	public static final String ERLANG_CONTEXT_TYPE= "org.erlide.ui.editor.erlang"; //$NON-NLS-1$

	/**
	 * Creates a new XML context type.
	 */
	public ErlangContextType() {
		addGlobalResolvers();
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
