package org.erlide.wrangler.refactoring.core.renameprocess;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.ltk.core.refactoring.Change;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.wrangler.refactoring.core.RPCMessage;
import org.erlide.wrangler.refactoring.core.RefactoringParameters;
import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.core.exception.WranglerException;
import org.erlide.wrangler.refactoring.core.exception.WranglerWarningException;
import org.erlide.wrangler.refactoring.core.functiontoprocess.ProcessRPCMessage;

import com.ericsson.otp.erlang.OtpErlangList;

public class RenameProcessRefactoring extends WranglerRefactoring {

	public RenameProcessRefactoring(RefactoringParameters parameters) {
		super(parameters);
	}

	@SuppressWarnings("boxing")
	@Override
	protected RpcResult sendRPC(String filePath, OtpErlangList searchPath)
			throws CoreException {
		Backend b = ErlangCore.getBackendManager().getIdeBackend();
		return b.call_noexception("wrangler", "rename_process_eclipse",
				"siisxi", parameters.getFilePath(), parameters.getStartLine(),
				parameters.getStartColumn(), newName, parameters
						.getSearchPath(), parameters.getEditorTabWidth());
	}

	@SuppressWarnings("boxing")
	protected RpcResult sendSecondRPC() throws CoreException {
		Backend b = ErlangCore.getBackendManager().getIdeBackend();
		return b.call_noexception("wrangler", "rename_process_1_eclipse",
				"sssxi", parameters.getFilePath(),
				((ProcessRPCMessage) message).getUndecidabels(), newName,
				parameters.getSearchPath(), parameters.getEditorTabWidth());
	}

	@Override
	protected RPCMessage convertRpcResultToRPCMessage(RpcResult res)
			throws WranglerException {
		RPCMessage m = new ProcessRPCMessage(res);
		message = m;
		try {
			m.checkIsOK();
		} catch (WranglerWarningException e) {
			throw new WranglerWarningException(
					"Wrangler could not decide whether the new process name provided\n"
							+ "conflicts with the process name(s) used by other"
							+ " registeration expression(s).");
		}

		return m;
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

	@Override
	public String getName() {
		return "Rename process";
	}

}
