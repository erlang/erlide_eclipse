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
package org.erlide.wrangler.refactoring.core;



import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.internal.ProcessRpcMessage;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

public abstract class ProcessRelatedRefactoring extends
		SimpleWranglerRefactoring {

	protected String undecidables;

	@Override
	public RefactoringStatus checkFinalConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		IErlSelection sel = GlobalParameters.getWranglerSelection();

		ProcessRpcMessage msg = checkUndecidables((IErlMemberSelection) sel);

		if (msg.isSuccessful()) {
			changedFiles = msg.getRefactoringChangeset();
			return new RefactoringStatus();
		} else if (msg.hasUndecidables()) {
			undecidables = msg.getMessageString();
			IRefactoringRpcMessage message = run(sel);
			changedFiles = message.getRefactoringChangeset();

			return RefactoringStatus
					.createWarningStatus(getUndecidableWarningMessage());
		} else {
			return RefactoringStatus.createFatalErrorStatus(msg.getMessageString());

		}
	}

	protected abstract String getUndecidableWarningMessage();

	protected abstract ProcessRpcMessage checkUndecidables(
			IErlMemberSelection sel);
}
