package org.erlide.wrangler.refactoring.tmp;

import java.util.ArrayList;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.swt.widgets.Shell;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.WranglerBackendManager;
import org.erlide.wrangler.refactoring.backend.internal.ExpressionPosRpcMessage;
import org.erlide.wrangler.refactoring.core.RefactoringWorkflowController;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;
import org.erlide.wrangler.refactoring.util.IErlRange;

import com.ericsson.otp.erlang.OtpErlangObject;

public class FoldExpressionRefactoring extends
		CostumWorkflowRefactoringWithPositionsSelection {

	protected OtpErlangObject syntaxTree;
	@Override
	public RefactoringStatus checkInitialConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {

		/*
		 * if (!((selection instanceof IErlMemberSelection) && (selection
		 * .getKind() == SelectionKind.FUNCTION || selection.getKind() ==
		 * SelectionKind.FUNCTION_CLAUSE))) return RefactoringStatus
		 * .createFatalError
		 */
		return new RefactoringStatus();

	}

	@Override
	public String getName() {
		return "Fold expression";
	}

	@Override
	public IRefactoringRpcMessage run(IErlSelection selection) {
		IErlMemberSelection sel = (IErlMemberSelection) selection;
		return WranglerBackendManager.getRefactoringBackend().call(
				"fold_expression_1_eclipse", "sxxxi", sel.getFilePath(),
				syntaxTree, getSelectedPos(), sel.getSearchPath(),
				GlobalParameters.getTabWidth());
	}

	@Override
	public RefactoringWorkflowController getWorkflowController(Shell shell) {
		return new RefactoringWorkflowController(shell) {

			@Override
			public void doRefactoring() {
				IErlSelection selection = GlobalParameters
						.getWranglerSelection();
				IErlMemberSelection sel = (IErlMemberSelection) selection;
				ExpressionPosRpcMessage m = new ExpressionPosRpcMessage();
				boolean answer = ask("Question",
						"Fold expression against the function clause pointed by cursor?");
				if (answer) {
					m = (ExpressionPosRpcMessage) WranglerBackendManager
							.getRefactoringBackend().callWithParser(m,
									"fold_expr_by_loc_eclipse", "siixi",
									sel.getFilePath(),
									sel.getMemberRange().getStartLine(),
									sel.getMemberRange().getStartCol(),
									sel.getSearchPath(),
									GlobalParameters.getTabWidth());
					if (m.isSuccessful()) {
						syntaxTree = m.getSyntaxTree();
						positions = m.getPositionDefinitions(sel.getDocument());
						selectedPositions = new ArrayList<IErlRange>();
						selectCandidates();
						run(selection);
					} else
						stop();

				} else {
					// getting user input
					String modName, funName, arity, clauseIndex;
				}

			}

			protected void selectCandidates() {

			}

		};
	}

	@Override
	public IRefactoringRpcMessage runAlternative(IErlSelection selection) {
		// TODO Auto-generated method stub
		return null;
	}

}
