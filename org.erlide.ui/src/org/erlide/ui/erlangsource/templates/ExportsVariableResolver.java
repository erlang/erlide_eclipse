/*******************************************************************************
 * Copyright (c) 2004 Lukas Larsson and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Lukas Larsson
 *******************************************************************************/

package org.erlide.ui.erlangsource.templates;

import java.util.ArrayList;
import java.util.Iterator;

import org.eclipse.jface.text.templates.TemplateContext;
import org.eclipse.jface.text.templates.TemplateVariable;

public class ExportsVariableResolver extends FunctionVariableResolver {

	private static final ArrayList<FunctionVariableResolver> fInstances = 
		new ArrayList<FunctionVariableResolver>();

	public ExportsVariableResolver() {
		fInstances.add(this);
	}

	public static ExportsVariableResolver getDefault() {
		if (fInstances.size() == 0) {
			fInstances.add(new ExportsVariableResolver());
		}
		return (ExportsVariableResolver) fInstances.get(0);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.ui.erlangsource.templates.FunctionVariableResolver#addFunction(java.lang.String,
	 *      int)
	 */
	public void addFunction(String name, int arity) {
		for (final Iterator iter = fInstances.iterator(); iter.hasNext();) {
			final ExportsVariableResolver element = (ExportsVariableResolver) iter
					.next();
			element.doAddFunction(name, arity);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.ui.erlangsource.templates.FunctionVariableResolver#clearFunctions()
	 */
	public void clearFunctions() {
		for (final Iterator iter = fInstances.iterator(); iter.hasNext();) {
			final ExportsVariableResolver element = (ExportsVariableResolver) iter
					.next();
			element.doClearFunctions();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.ui.erlangsource.templates.FunctionVariableResolver#resolve(org.eclipse.jface.text.templates.TemplateVariable,
	 *      org.eclipse.jface.text.templates.TemplateContext)
	 */
	@Override
	public void resolve(TemplateVariable variable, TemplateContext context) {
		final StringBuffer buff = new StringBuffer();

		for (final Iterator iter = functions.iterator(); iter.hasNext();) {
			final String[] part = buff.toString().split("\\n");
			if (part[part.length - 1].length() > 60) {
				buff.append("\n");
			}
			final Object[] element = (Object[]) iter.next();
			buff.append(element[0] + "/" + element[1]);
			if (iter.hasNext()) {
				buff.append(", ");
			}
		}

		variable.setValue(buff.toString());
	}

}
