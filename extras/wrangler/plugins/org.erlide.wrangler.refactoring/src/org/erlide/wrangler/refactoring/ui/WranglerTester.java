/*******************************************************************************
 * Copyright (c) 2010 György Orosz. All rights reserved. This program and the accompanying
 * materials are made available under the terms of the Eclipse Public License v1.0 which
 * accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: György Orosz - initial API and implementation
 ******************************************************************************/
package org.erlide.wrangler.refactoring.ui;

import org.eclipse.core.expressions.PropertyTester;

/**
 * Property tester class for checking menu visibilty preconditions
 *
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class WranglerTester extends PropertyTester {
    /**
     * Default constructor
     */
    public WranglerTester() {
    }

    /**
     * Test if QC, GraphViz is installed on the system.
     */

    @Override
    public boolean test(final Object receiver, final String property, final Object[] args,
            final Object expectedValue) {
        if ("hasQuickCheck".equals(property) || "hasGraphViz".equals(property)) {
            return true;// GlobalParameters.hasQuickCheck();
        }

        return true;
    }
}
