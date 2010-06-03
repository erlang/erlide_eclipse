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
package org.erlide.wrangler.refactoring.ui;

import org.eclipse.core.expressions.PropertyTester;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

/**
 * Property tester class for checking if QuickCheck is installed on the system
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class QuickCheckTester extends PropertyTester {
	/**
	 * Default constructor
	 */
	public QuickCheckTester() {
	}

	/**
	 * Test if QC is installed on the system.
	 */
	public boolean test(final Object receiver, final String property,
			final Object[] args, final Object expectedValue) {
		if (property.equals("hasQuickCheck")) {
			return GlobalParameters.hasQuickCheck();
		}
		return true;
	}

}
