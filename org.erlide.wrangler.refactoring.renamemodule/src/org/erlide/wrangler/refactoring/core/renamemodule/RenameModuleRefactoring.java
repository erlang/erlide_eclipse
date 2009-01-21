package org.erlide.wrangler.refactoring.core.renamemodule;

import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.resource.RenameResourceChange;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.RpcResult;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.wrangler.refactoring.core.FileResourceChanges;
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
		return managedBackend.rpc("wrangler", "rename_mod_eclipse", "ssx",
				filePath, newName, searchPath);
	}

	@Override
	protected Change doOtherChanges() {
		CompositeChange ch = new CompositeChange("Renamings");
		List<FileResourceChanges> changedFiles = message.getResult();
		for (FileResourceChanges f : changedFiles) {
			if (f.isNameChanged()) {
				IPath p = f.getIPath();
				String s = f.getNewName();
				RenameResourceChange rch = new RenameResourceChange(p, s);
				ch.add(rch);
			}
		}

		return ch;
	}
}
