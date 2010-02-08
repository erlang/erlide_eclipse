package org.erlide.wrangler.refactoring.tmp;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.swt.widgets.Shell;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.RefactoringState;
import org.erlide.wrangler.refactoring.backend.WranglerBackendManager;
import org.erlide.wrangler.refactoring.backend.internal.GenFunRefactoringMessage;
import org.erlide.wrangler.refactoring.core.RefactoringWorkflowController;
import org.erlide.wrangler.refactoring.core.internal.CostumWorkflowRefactoringWithPositionsSelection;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection.SelectionKind;
import org.erlide.wrangler.refactoring.util.ErlRange;
import org.erlide.wrangler.refactoring.util.GlobalParameters;
import org.erlide.wrangler.refactoring.util.IErlRange;
import org.erlide.wrangler.refactoring.util.Range;

import com.ericsson.otp.erlang.OtpErlangBoolean;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

//really ugly workflow!!! :(
//sorry for the continuous hacking, but it is impossible to do it in a nice way
public class GeneraliseFunctionRefactoring extends
		CostumWorkflowRefactoringWithPositionsSelection {
	protected Map<String, OtpErlangObject> parameters = null;
	protected boolean hasSideEffect;
	protected RefactoringStatus status = null;
	protected boolean doFirstCase;

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
		} else if (parameters != null) {
			IRefactoringRpcMessage msg;
			if (doFirstCase) {
				msg = run(sel);
			} else {
				msg = runAlternative(sel);
			}

			if (msg.isSuccessful()) {
				changedFiles = msg.getRefactoringChangeset();
				return new RefactoringStatus();
			} else {
				return RefactoringStatus.createFatalErrorStatus(msg
						.getMessageString());
			}
		}
		return new RefactoringStatus();

	}

	protected HashMap<IErlRange, OtpErlangTuple> parsePositions(
			OtpErlangList positions, IDocument doc) {
		HashMap<IErlRange, OtpErlangTuple> ret = new HashMap<IErlRange, OtpErlangTuple>();

		OtpErlangObject[] positionArray = positions.elements();
		for (OtpErlangObject o : positionArray) {
			try {
				ret.put(new ErlRange(new Range((OtpErlangTuple) o), doc),
						(OtpErlangTuple) o);
			} catch (OtpErlangRangeException e) {
				e.printStackTrace();
			}
		}

		return ret;
	}

	@Override
	public String getName() {
		return "Generalise function";
	}

	protected GenFunRefactoringMessage preRun(IErlMemberSelection sel) {
		return (GenFunRefactoringMessage) WranglerBackendManager
				.getRefactoringBackend().callWithParser(
						new GenFunRefactoringMessage(), "generalise_eclipse",
						"sxxsxi", sel.getFilePath(),
						sel.getSelectionRange().getStartPos(),
						sel.getSelectionRange().getEndPos(), userInput,
						sel.getSearchPath(), GlobalParameters.getTabWidth());

	}

	@Override
	public IRefactoringRpcMessage run(IErlSelection selection) {
		IErlMemberSelection sel = (IErlMemberSelection) selection;
		return WranglerBackendManager.getRefactoringBackend().call(
				"gen_fun_1_eclipse", "xsxxxxxix",
				new OtpErlangBoolean(this.hasSideEffect), sel.getFilePath(),
				parameters.get("funName"), parameters.get("arity"),
				parameters.get("funDefPos"), parameters.get("exp"),
				getSelectedPos(), sel.getSearchPath(),
				GlobalParameters.getTabWidth(), parameters.get("logCmd"));
	}

	@Override
	public IRefactoringRpcMessage runAlternative(IErlSelection selection) {
		IErlMemberSelection sel = (IErlMemberSelection) selection;
		return WranglerBackendManager.getRefactoringBackend().call(
				"gen_fun_eclipse", "sxxxxxxx", sel.getFilePath(),
				parameters.get("parName"), parameters.get("funName"),
				parameters.get("arity"), parameters.get("funDefPos"),
				parameters.get("exp"), GlobalParameters.getTabWidth(),
				new OtpErlangBoolean(hasSideEffect), getSelectedPos());
	}

	/*
	 * public void setHasSideEffect(boolean b) { this.hasSideEffect = b; }
	 */

	/*
	 * public boolean isUserInteractionNeeded() { IErlMemberSelection sel =
	 * (IErlMemberSelection) GlobalParameters .getWranglerSelection();
	 * SideEffectRefactoringMessage msg = trySideEffect(sel); syntaxinfo = null;
	 * if (msg.isSuccessful()) { this.changedFiles =
	 * msg.getRefactoringChangeset(); return false; } else if
	 * (msg.isUserInteractionNeeded()) { syntaxinfo = msg.getSyntaxInfo();
	 * return true; } else { this.status =
	 * RefactoringStatus.createFatalErrorStatus(msg .getMessageString()); return
	 * false; }
	 * 
	 * }
	 */

	@Override
	public RefactoringWorkflowController getWorkflowController(Shell shell) {
		return new RefactoringWorkflowController(shell) {
			boolean controlPageOrder = false;

			@Override
			public void doRefactoring() {
				IErlMemberSelection selection = (IErlMemberSelection) GlobalParameters
						.getWranglerSelection();
				GenFunRefactoringMessage msg = preRun(selection);
				if (msg.getRefactoringState() == RefactoringState.ERROR) {
					status = RefactoringStatus.createFatalErrorStatus(msg
							.getMessageString());
				} else if (msg.getRefactoringState() == RefactoringState.OK) {
					controlPageOrder = true;
					changedFiles = msg.getRefactoringChangeset();
					status = new RefactoringStatus();
				} else if (msg.getRefactoringState() == RefactoringState.UNKNOWN_SIDE_EFFECT) {
					hasSideEffect = ask("Question",
							"Does the selected expression have side effect?");
					OtpErlangInt noOfClauses = (OtpErlangInt) msg
							.getParameters().get("noOfClauses");
					try {
						if (noOfClauses.intValue() > 1)
							chooseFromSelectionTypes(msg, selection);
						else
							doSelectionDupsinFun(msg, selection);
					} catch (OtpErlangRangeException e) {
						status = RefactoringStatus
								.createFatalErrorStatus("Internal error!\n Please report it!");
					}
				} else if (msg.getRefactoringState() == RefactoringState.MULTI_INSTANCES) {
					doSelectionDupsinFun(msg, selection);

				} else if (msg.getRefactoringState() == RefactoringState.MORE_THAN_ONE_CLAUSE) {
					chooseFromSelectionTypes(msg, selection);
				}

			}

			protected void doSelectionDupsinFun(GenFunRefactoringMessage msg,
					IErlMemberSelection sel) {
				doFirstCase = true;
				positions = parsePositions((OtpErlangList) parameters
						.get("dupsInFun"), sel.getDocument());
			}

			protected void chooseFromSelectionTypes(
					GenFunRefactoringMessage msg, IErlMemberSelection sel) {
				if (ask(
						"Question",
						"The function selected has multiple clauses, would you like to generalise the function clause selected only?")) {
					doSelectionDupsinFun(msg, sel);
				} else {
					doSelectionDupsinClause(msg, sel);
				}

			}

			protected void doSelectionDupsinClause(
					GenFunRefactoringMessage msg, IErlMemberSelection sel) {
				doFirstCase = false;
				positions = parsePositions((OtpErlangList) parameters
						.get("dupsInClause"), sel.getDocument());
			}

		};
	}

}
