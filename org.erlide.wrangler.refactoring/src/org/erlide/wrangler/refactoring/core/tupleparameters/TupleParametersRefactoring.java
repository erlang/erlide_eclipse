package org.erlide.wrangler.refactoring.core.tupleparameters;

import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.wrangler.refactoring.core.RefactoringParameters;
import org.erlide.wrangler.refactoring.core.WranglerRefactoring;

import com.ericsson.otp.erlang.OtpErlangList;

public class TupleParametersRefactoring extends WranglerRefactoring {

	public TupleParametersRefactoring(RefactoringParameters parameters) {
		super(parameters);
	}

	@Override
	public String getName() {
		return "Tuple function parameters";
	}

	@SuppressWarnings("boxing")
	@Override
	protected RpcResult sendRPC(String filePath, OtpErlangList searchPath) {
		return managedBackend.call_noexception("wrangler",
				"tuple_funpar_eclipse", "siisxi", filePath, parameters
						.getStartLine(), parameters.getStartColumn(), newName,
				searchPath, parameters.getEditorTabWidth());
	}

}
