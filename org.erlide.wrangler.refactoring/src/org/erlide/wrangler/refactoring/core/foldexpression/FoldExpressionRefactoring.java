package org.erlide.wrangler.refactoring.core.foldexpression;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.wrangler.refactoring.core.RPCMessage;
import org.erlide.wrangler.refactoring.core.RefactoringParameters;
import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.core.exception.WranglerException;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class FoldExpressionRefactoring extends WranglerRefactoring {

	private OtpErlangList selectedPositions;

	private OtpErlangList foundPositions;

	private OtpErlangObject funClauseDef;

	public FoldExpressionRefactoring(RefactoringParameters parameters) {
		super(parameters);
	}

	public void setFunClauseDef(OtpErlangObject o) {
		funClauseDef = o;
	}

	public void setSelectedPositions(List<OtpErlangTuple> pos) {
		Object[] objects = pos.toArray();
		OtpErlangObject[] otpObjects = new OtpErlangObject[objects.length];
		for (int i = 0; i < objects.length; ++i) {
			otpObjects[i] = (OtpErlangObject) objects[i];
		}
		selectedPositions = new OtpErlangList(otpObjects);
	}

	public List<OtpErlangTuple> getFoundPositions() {
		ArrayList<OtpErlangTuple> res = new ArrayList<OtpErlangTuple>();

		for (OtpErlangObject o : foundPositions.elements()) {
			res.add((OtpErlangTuple) o);
		}

		return res;
	}

	public void setFoundPositions(OtpErlangList l) {
		foundPositions = l;
	}

	@SuppressWarnings("boxing")
	@Override
	protected RpcResult sendRPC(String filePath, OtpErlangList searchPath)
			throws CoreException {
		Backend b = BackendManager.getDefault().getIdeBackend();
		return b.call_noexception("wrangler", "fold_expression_1_eclipse",
				"sxxxi", filePath, funClauseDef, selectedPositions, parameters
						.getSearchPath(), parameters.getEditorTabWidth());
	}

	@SuppressWarnings("boxing")
	private RPCMessage callFoldExpression() throws WranglerException,
			CoreException {
		Backend b = BackendManager.getDefault().getIdeBackend();
		RpcResult res = b.call_noexception("wrangler",
				"fold_expr_by_loc_eclipse", "siixi", parameters.getFilePath(),
				parameters.getStartLine(), parameters.getStartColumn(),
				parameters.getSearchPath(), parameters.getEditorTabWidth());
		FoldExpressionRPCMessage m = new FoldExpressionRPCMessage(res, this);
		m.checkIsOK();
		return m;

	}

	@Override
	public String getName() {
		return "Fold expression";
	}

	@Override
	public RefactoringStatus checkInitialConditions(IProgressMonitor m) {
		RefactoringStatus rs = new RefactoringStatus();
		try {
			callFoldExpression();
			// extract information
		} catch (WranglerException e) {
			String s = e.getLocalizedMessage();
			rs = RefactoringStatus.createFatalErrorStatus(s);
		} catch (CoreException e) {
			rs = RefactoringStatus.createFatalErrorStatus(e.getMessage());
		}
		return rs;
	}

}
