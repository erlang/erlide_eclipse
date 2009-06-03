package org.erlide.wrangler.refactoring.core.functiontoprocess;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.ltk.core.refactoring.Change;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.wrangler.refactoring.core.RPCMessage;
import org.erlide.wrangler.refactoring.core.RefactoringParameters;
import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.core.exception.WranglerException;

import com.ericsson.otp.erlang.OtpErlangList;

public class FunctionToProcessRefactoring extends WranglerRefactoring {

	public FunctionToProcessRefactoring(RefactoringParameters parameters) {
		super(parameters);
	}

	@SuppressWarnings("boxing")
	@Override
	protected RpcResult sendRPC(String filePath, OtpErlangList searchPath)
			throws CoreException {
		Backend b = BackendManager.getDefault().getIdeBackend();
		return b.call_noexception("wrangler", "fun_to_process_eclipse",
				"siisxi", parameters.getFilePath(), parameters.getStartLine(),
				parameters.getStartColumn(), newName, parameters
						.getSearchPath(), parameters.getEditorTabWidth());
	}

	@SuppressWarnings("boxing")
	protected RpcResult sendSecondRPC() throws CoreException {
		Backend b = BackendManager.getDefault().getIdeBackend();
		return b.call_noexception("wrangler", "fun_to_process_1_eclipse",
				"siisxi", parameters.getFilePath(), parameters.getStartLine(),
				parameters.getStartColumn(), newName, parameters
						.getSearchPath(), parameters.getEditorTabWidth());
	}

	@Override
	protected RPCMessage convertRpcResultToRPCMessage(RpcResult res)
			throws WranglerException {
		RPCMessage m = new ProcessRPCMessage(res);
		message = m;
		m.checkIsOK();
		return m;
	}

	@Override
	public String getName() {
		return "Function to process";
	}

	@Override
	public Change createChange(IProgressMonitor pm) throws CoreException,
			OperationCanceledException {
		RpcResult res = sendSecondRPC();
		message = new RPCMessage(res);
		try {
			message.checkIsOK();
		} catch (WranglerException e) {
			throw new CoreException(new Status(IStatus.ERROR,
					org.erlide.wrangler.refactoring.Activator.PLUGIN_ID, e
							.getMessage()));

		}
		return super.createChange(pm);
	}

}
