package org.erlide.wrangler.refactoring.core.renamemodule;

import org.eclipse.ltk.core.refactoring.Change;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.RpcResult;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.wrangler.refactoring.core.RefactoringParameters;
import org.erlide.wrangler.refactoring.core.WranglerRefactoring;

import com.ericsson.otp.erlang.OtpErlangList;

public class RenameModuleRefactoring extends WranglerRefactoring {

	public RenameModuleRefactoring(RefactoringParameters parameters) {
		super(parameters);
	}

	@Override
	public String getName() {
		return "Rename module";
	}

	@Override
	protected RpcResult sendRPC(String filePath, OtpErlangList searchPath)
			throws ErlangRpcException, RpcException {
		// TODO: not fully working
		return managedBackend.rpc("wrangler", "rename_mod_eclipse", "ssx",
				filePath, newName, searchPath);
	}

	@Override
	protected Change doOtherChanges() {
		org.eclipse.ltk.internal.core.refactoring.Resources r;
		return null;
	}
}
