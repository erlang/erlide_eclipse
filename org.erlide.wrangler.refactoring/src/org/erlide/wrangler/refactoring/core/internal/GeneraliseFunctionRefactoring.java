package org.erlide.wrangler.refactoring.core.internal;



import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.WranglerBackendManager;
import org.erlide.wrangler.refactoring.backend.internal.SideEffectRefactoringMessage;
import org.erlide.wrangler.refactoring.core.SimpleWranglerRefactoring;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection.SelectionKind;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

import com.ericsson.otp.erlang.OtpErlangBoolean;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class GeneraliseFunctionRefactoring extends SimpleWranglerRefactoring {
	protected OtpErlangTuple syntaxinfo = null;
	protected boolean hasSideEffect;
	protected RefactoringStatus status = null;

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
		// TODO: testing
		return RefactoringStatus
				.createFatalErrorStatus("Please select an expression!");
	}

	@Override
	public RefactoringStatus checkFinalConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		IErlMemberSelection sel = (IErlMemberSelection) GlobalParameters
				.getWranglerSelection();
		if (status != null) {
			return status;
		} else if (syntaxinfo != null) {
			IRefactoringRpcMessage message = run(sel);
			this.changedFiles = message.getRefactoringChangeset();
		}
		return new RefactoringStatus();

	}

	@Override
	public String getName() {
		return "Generalise function";
	}

	public SideEffectRefactoringMessage trySideEffect(IErlMemberSelection sel) {
		return (SideEffectRefactoringMessage) WranglerBackendManager
				.getRefactoringBackend().callWithParser(
						new SideEffectRefactoringMessage(),
						"generalise_eclipse", "sxxsxi", sel.getFilePath(),
						sel.getSelectionRange().getStartPos(),
						sel.getSelectionRange().getEndPos(), userInput,
						sel.getSearchPath(), GlobalParameters.getTabWidth());

	}

	@Override
	public IRefactoringRpcMessage run(IErlSelection selection) {
		IErlMemberSelection sel = (IErlMemberSelection) selection;
		return WranglerBackendManager.getRefactoringBackend().call(
				"gen_fun_1_eclipse", "xsxxxxxi",
				new OtpErlangBoolean(this.hasSideEffect), sel.getFilePath(),
				syntaxinfo.elementAt(0), syntaxinfo.elementAt(1),
				syntaxinfo.elementAt(2), syntaxinfo.elementAt(3),
				syntaxinfo.elementAt(4), GlobalParameters.getTabWidth());
	}

	public void setHasSideEffect(boolean b) {
		this.hasSideEffect = b;
	}

	public boolean isUserInteractionNeeded() {
		IErlMemberSelection sel = (IErlMemberSelection) GlobalParameters
				.getWranglerSelection();
		SideEffectRefactoringMessage msg = trySideEffect(sel);
		syntaxinfo = null;
		if (msg.isSuccessful()) {
			this.changedFiles = msg.getRefactoringChangeset();
			return false;
		} else if (msg.isUserInteractionNeeded()) {
			syntaxinfo = msg.getSyntaxInfo();
			return true;
		} else {
			this.status = RefactoringStatus.createFatalErrorStatus(msg
					.getMessageString());
			return false;
		}

	}
}
