package org.erlide.wrangler.refactoring.core.movefunction;

import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.wrangler.refactoring.core.RefactoringParameters;
import org.erlide.wrangler.refactoring.core.WranglerRefactoring;

import com.ericsson.otp.erlang.OtpErlangBoolean;
import com.ericsson.otp.erlang.OtpErlangList;

public class MoveFunctionRefactoring extends WranglerRefactoring {

	boolean isNewModule;

	public void setIsNewModule(boolean b) {
		isNewModule = b;
	}

	public MoveFunctionRefactoring(RefactoringParameters parameters) {
		super(parameters);
	}

	@Override
	public String getName() {
		return "Move function";
	}

	@SuppressWarnings("boxing")
	@Override
	protected RpcResult sendRPC(String filePath, OtpErlangList searchPath) {
		Backend b = BackendManager.getDefault().getIdeBackend();
		return b.call_noexception("wrangler", "move_fun_eclipse", "siisxxi",
				filePath, parameters.getStartLine(), parameters
						.getStartColumn(), newName, new OtpErlangBoolean(
						isNewModule), searchPath, parameters
						.getEditorTabWidth());
	}
}
