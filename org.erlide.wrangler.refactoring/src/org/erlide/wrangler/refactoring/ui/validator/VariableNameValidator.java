/*******************************************************************************
 * Copyright (c) 2010 György Orosz.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     György Orosz - initial API and implementation
 ******************************************************************************/
package org.erlide.wrangler.refactoring.ui.validator;

public class VariableNameValidator implements IValidator {

	public boolean isValid(String s) {
		if (s.length() == 0) {
			return false;
		}

		if (s.startsWith("_")
				|| s.substring(0, 1).toUpperCase().equals(s.substring(0, 1))) {
			if (s.replaceAll("[A-Za-z_@0-9]", "").length() == 0) {
				return true;
			} else {
				return false;
			}
		} else {
			return false;
		}
	}

}
