package org.erlide.wrangler.refactoring.core.renamemodule;

import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.resource.RenameResourceChange;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.rpc.RpcResult;
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

	@SuppressWarnings("boxing")
	@Override
	protected RpcResult sendRPC(String filePath, OtpErlangList searchPath) {
		Backend b = ErlangCore.getBackendManager().getIdeBackend();
		return b.call_noexception("wrangler", "rename_mod_eclipse", "ssxi",
				filePath, newName, searchPath, parameters.getEditorTabWidth());
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
