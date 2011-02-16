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

import org.eclipse.ltk.core.refactoring.Refactoring;
import org.eclipse.ltk.ui.refactoring.RefactoringWizard;

/**
 * Abstract wizard which should be used for integrating wrangler refactoringss
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public abstract class AbstractWranglerRefactoringWizard extends
        RefactoringWizard {

    /**
     * Default constructor
     * 
     * @param refactoring
     *            refactoring
     * @param flags
     *            flags
     */
    public AbstractWranglerRefactoringWizard(final Refactoring refactoring,
            final int flags) {
        super(refactoring, flags);
    }

}
