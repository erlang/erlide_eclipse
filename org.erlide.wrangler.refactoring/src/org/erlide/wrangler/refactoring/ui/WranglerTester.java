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
    public boolean test(final Object receiver, final String property,
            final Object[] args, final Object expectedValue) {
        if (property.equals("hasQuickCheck")) {
            return true;// GlobalParameters.hasQuickCheck();
        } else if ("hasGraphViz".equals(property)) {

            // Bundle[] bs = Platform
            // .getFragments(Platform
            // .getBundle(org.erlide.wrangler.refactoring.Activator.PLUGIN_ID));
            //
            // for (int i = 0; i < bs.length; ++i) {
            // if (bs[i].getSymbolicName().equals(
            // "org.erlide.wrangler.refactoring.codeinspection")) {
            //
            // return true;
            // }
            // }

            return true;
        }

        return true;
    }
}
