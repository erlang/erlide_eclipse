package org.erlide.wrangler.refactoring.core.funextraction;

import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.wrangler.refactoring.core.RefactoringParameters;
import org.erlide.wrangler.refactoring.core.WranglerRefactoring;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class FunExtractionRefactoring extends WranglerRefactoring {

	public FunExtractionRefactoring(RefactoringParameters parameters) {
		super(parameters);
	}

	@Override
	public String getName() {
		return "Fun extraction";
	}

	@SuppressWarnings("boxing")
	@Override
	protected RpcResult sendRPC(String filePath, OtpErlangList searchPath) {
		OtpErlangTuple startPos = createPos(parameters.getStartLine(),
				parameters.getStartColumn());
		OtpErlangTuple endPos = createPos(parameters.getEndLine(), parameters
				.getEndColumn());
		Backend b = ErlangCore.getBackendManager().getIdeBackend();
		return b.call_noexception("wrangler", "fun_extraction_eclipse",
				"sxxsi", filePath, startPos, endPos, newName, parameters
						.getEditorTabWidth());
	}

}
