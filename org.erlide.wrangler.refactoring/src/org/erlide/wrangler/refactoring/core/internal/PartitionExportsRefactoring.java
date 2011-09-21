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
package org.erlide.wrangler.refactoring.core.internal;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;
import org.erlide.wrangler.refactoring.core.SimpleOneStepWranglerRefactoring;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

/**
 * Integration class of the partition_exports_eclipse wrangler refactoring
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class PartitionExportsRefactoring extends
        SimpleOneStepWranglerRefactoring {

    @Override
    public RefactoringStatus checkInitialConditions(final IProgressMonitor pm)
            throws CoreException, OperationCanceledException {

        return new RefactoringStatus();
    }

    @Override
    public String getName() {
        return "Partition exported functions";
    }

    @Override
    public IRefactoringRpcMessage run(final IErlSelection sel) {
        return WranglerBackendManager.getRefactoringBackend().call(
                "partition_exports_eclipse", "sdxi", sel.getFilePath(),
                Double.parseDouble(userInput), sel.getSearchPath(),
                GlobalParameters.getTabWidth());
    }
}
