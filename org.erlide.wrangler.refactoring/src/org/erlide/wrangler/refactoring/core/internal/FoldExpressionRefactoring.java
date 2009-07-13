package org.erlide.wrangler.refactoring.core.internal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.WranglerBackendManager;
import org.erlide.wrangler.refactoring.backend.internal.ExpressionPosRpcMessage;
import org.erlide.wrangler.refactoring.core.SimpleOneStepWranglerRefactoring;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection.SelectionKind;
import org.erlide.wrangler.refactoring.util.GlobalParameters;
import org.erlide.wrangler.refactoring.util.IErlRange;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class FoldExpressionRefactoring extends SimpleOneStepWranglerRefactoring {

	protected OtpErlangObject syntaxTree;
	protected HashMap<IErlRange, OtpErlangTuple> positions;
	protected ArrayList<IErlRange> selectedPositions;

	@Override
	public RefactoringStatus checkInitialConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {

		IErlSelection selection = GlobalParameters.getWranglerSelection();

		if (!((selection instanceof IErlMemberSelection) && (selection
				.getKind() == SelectionKind.FUNCTION || selection.getKind() == SelectionKind.FUNCTION_CLAUSE)))
			return RefactoringStatus
					.createFatalErrorStatus("Please select an expression!");

		IErlMemberSelection sel = (IErlMemberSelection) selection;
		ExpressionPosRpcMessage m = new ExpressionPosRpcMessage();
		m = (ExpressionPosRpcMessage) WranglerBackendManager
				.getRefactoringBackend().callWithParser(m,
						"fold_expr_by_loc_eclipse", "siixi", sel.getFilePath(),
						sel.getMemberRange().getStartLine(),
						sel.getMemberRange().getStartCol(),
						sel.getSearchPath(), GlobalParameters.getTabWidth());
		if (m.isSuccessful()) {
			syntaxTree = m.getSyntaxTree();
			positions = m.getPositionDefinitions(sel.getDocument());
			selectedPositions = new ArrayList<IErlRange>();
			return new RefactoringStatus();
		} else
			return RefactoringStatus.createFatalErrorStatus(m.getMessage());

	}

	public List<IErlRange> getPositions() {
		ArrayList<IErlRange> ret = new ArrayList<IErlRange>();
		for (IErlRange r : positions.keySet()) {
			ret.add(r);
		}

		return ret;
	}

	public void setSelectedPos(ArrayList<IErlRange> l) {
		selectedPositions = l;
	}

	@Override
	public String getName() {
		return "Fold expression";
	}

	protected OtpErlangList getErlangSelectedPos() {
		OtpErlangList ret;
		OtpErlangObject[] selection = new OtpErlangObject[selectedPositions
				.size()];

		for (int i = 0; i < selectedPositions.size(); ++i) {
			selection[i] = positions.get(selectedPositions.get(i));
		}

		ret = new OtpErlangList(selection);
		return ret;
	}

	@Override
	public IRefactoringRpcMessage run(IErlSelection selection) {
		IErlMemberSelection sel = (IErlMemberSelection) selection;
		return WranglerBackendManager.getRefactoringBackend().call(
				"fold_expression_1_eclipse", "sxxxi", sel.getFilePath(),
				syntaxTree, getErlangSelectedPos(), sel.getSearchPath(),
				GlobalParameters.getTabWidth());
	}

}
