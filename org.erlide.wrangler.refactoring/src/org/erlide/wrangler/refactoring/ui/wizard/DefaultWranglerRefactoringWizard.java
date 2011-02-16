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
package org.erlide.wrangler.refactoring.ui.wizard;

import java.util.ArrayList;

import org.eclipse.ltk.core.refactoring.Refactoring;
import org.erlide.wrangler.refactoring.ui.wizardpages.WranglerPage;

/**
 * Refactoring Wizard class which used for integrating Wrangler refactorings
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class DefaultWranglerRefactoringWizard extends
        AbstractWranglerRefactoringWizard {

    private final ArrayList<WranglerPage> pages;

    /**
     * Default constructor
     * 
     * @param refactoring
     *            integrated refactoring
     * @param flags
     *            flags
     * @param pages
     *            input pages which should be shown
     */
    public DefaultWranglerRefactoringWizard(final Refactoring refactoring,
            final int flags, final ArrayList<WranglerPage> pages) {
        super(refactoring, flags);
        this.pages = pages;
        setWindowTitle(refactoring.getName());
        setDefaultPageTitle(refactoring.getName());
    }

    @Override
    protected void addUserInputPages() {
        for (final WranglerPage page : pages) {
            addPage(page);
        }
    }
}
