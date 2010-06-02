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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.WranglerBackendManager;
import org.erlide.wrangler.refactoring.core.SimpleOneStepWranglerRefactoring;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection.SelectionKind;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

import com.ericsson.otp.erlang.OtpErlangBoolean;

/**
 * Normalize record expression refactoring's integration
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class NormalizeRecordExpression extends SimpleOneStepWranglerRefactoring {

	private boolean showDefault;

	/**
	 * @param showDefault
	 *            indicates whether the refactoring should include default
	 *            fields
	 */
	public NormalizeRecordExpression(final boolean showDefault) {
		this.showDefault = showDefault;
	}

	@Override
	public RefactoringStatus checkInitialConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		IErlSelection sel = GlobalParameters.getWranglerSelection();
		if (sel instanceof IErlMemberSelection) {
			SelectionKind kind = sel.getKind();
			if (kind == SelectionKind.FUNCTION_CLAUSE
					|| kind == SelectionKind.FUNCTION)
				return new RefactoringStatus();
		}

		return RefactoringStatus
				.createFatalErrorStatus("Please select a record expression!");
	}

	@Override
	public String getName() {
		return "Normalize record expression";
	}

	@Override
	public IRefactoringRpcMessage run(IErlSelection selection) {
		IErlMemberSelection sel = (IErlMemberSelection) selection;
		return WranglerBackendManager.getRefactoringBackend().call(
				"normalise_record_expr_eclipse", "sxxxi", sel.getFilePath(),
				sel.getSelectionRange().getStartPos(),
				new OtpErlangBoolean(this.showDefault), sel.getSearchPath(),
				GlobalParameters.getTabWidth());

	}
}
