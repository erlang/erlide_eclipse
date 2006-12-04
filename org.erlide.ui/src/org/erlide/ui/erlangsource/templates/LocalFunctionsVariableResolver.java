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

public class LocalFunctionsVariableResolver extends FunctionVariableResolver {

	private static final ArrayList<FunctionVariableResolver> fInstances = new ArrayList<FunctionVariableResolver>();

	public LocalFunctionsVariableResolver() {
		fInstances.add(this);
	}

	public static LocalFunctionsVariableResolver getDefault() {
		if (fInstances.size() == 0) {
			fInstances.add(new LocalFunctionsVariableResolver());
		}
		return (LocalFunctionsVariableResolver) fInstances.get(0);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.ui.erlangsource.templates.FunctionVariableResolver#addFunction(java.lang.String,
	 *      int)
	 */
	public void addFunction(String name, int arity) {
		for (final Iterator iter = fInstances.iterator(); iter.hasNext();) {
			final LocalFunctionsVariableResolver element = (LocalFunctionsVariableResolver) iter
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
			final LocalFunctionsVariableResolver element = (LocalFunctionsVariableResolver) iter
					.next();
			element.doClearFunctions();
		}
	}

}