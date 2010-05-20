package org.erlide.wrangler.refactoring.core.internal;

import java.util.ArrayList;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.swt.widgets.Shell;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlFunctionClause;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.WranglerBackendManager;
import org.erlide.wrangler.refactoring.backend.internal.ExpressionPosRpcMessage;
import org.erlide.wrangler.refactoring.core.CostumWorkflowRefactoringWithPositionsSelection;
import org.erlide.wrangler.refactoring.core.RefactoringWorkflowController;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;
import org.erlide.wrangler.refactoring.util.IErlRange;

import com.ericsson.otp.erlang.OtpErlangObject;

public class FoldRemoteExpressionRefactoring extends
		CostumWorkflowRefactoringWithPositionsSelection {

	// the selected module, on which, the refactoring will be applied
	private IErlMemberSelection selection;
	// the selected function clause, which is applied on the module
	private IErlFunctionClause functionClause;
	protected OtpErlangObject syntaxTree;

	public FoldRemoteExpressionRefactoring(IErlFunctionClause functionClause,
			IErlMemberSelection selection) {
		this.functionClause = functionClause;
		this.selection = selection;
	}

	@Override
	public RefactoringWorkflowController getWorkflowController(Shell shell) {
		return null;
	}

	@Override
	public IRefactoringRpcMessage runAlternative(IErlSelection selection) {
		return null;
	}

	@Override
	public RefactoringStatus checkInitialConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		if (functionClause == null) {
			return RefactoringStatus
					.createFatalErrorStatus("No function clause was given!");
		} else {

			ExpressionPosRpcMessage m = new ExpressionPosRpcMessage();
			String path = selection.getFilePath();
			String moduleName = functionClause.getModule().getModuleName();
			String functionName = functionClause.getFunctionName();
			int arity = functionClause.getArity();

			int clauseIndex = 1;
			if (!(functionClause instanceof IErlFunction))
				// FIXME: avoid hacking!!!
				clauseIndex = Integer.valueOf(functionClause.getName()
						.substring(1));

			m = (ExpressionPosRpcMessage) WranglerBackendManager
					.getRefactoringBackend().callWithParser(m,
							"fold_expr_by_name_eclipse", "sssiixi", path,
							moduleName, functionName, arity, clauseIndex,
							selection.getSearchPath(),
							GlobalParameters.getTabWidth());

			if (m.isSuccessful()) {
				syntaxTree = m.getSyntaxTree();
				// TODO: store positions, selectedpositions
				positions = m.getPositionDefinitions(selection.getDocument());
				selectedPositions = new ArrayList<IErlRange>();
			} else {
				return RefactoringStatus.createFatalErrorStatus(m
						.getMessageString());
			}
		}

		return new RefactoringStatus();
	}

	@Override
	public String getName() {
		return "Fold expression";
	}

	@Override
	public RefactoringStatus checkFinalConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		IErlSelection sel = GlobalParameters.getWranglerSelection();
		IRefactoringRpcMessage message = run(sel);
		if (message.isSuccessful()) {
			changedFiles = message.getRefactoringChangeset();
			return new RefactoringStatus();
		} else {
			return RefactoringStatus.createFatalErrorStatus(message
					.getMessageString());
		}
	}

	@Override
	public IRefactoringRpcMessage run(IErlSelection selection) {
		IErlMemberSelection sel = (IErlMemberSelection) selection;
		return WranglerBackendManager.getRefactoringBackend().call(
				"fold_expr_1_eclipse", "sxxxi", sel.getFilePath(), syntaxTree,
				getSelectedPos(), sel.getSearchPath(),
				GlobalParameters.getTabWidth());
	}

}
