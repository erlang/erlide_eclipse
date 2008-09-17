package org.erlide.wrangler.refactoring.core.tupleparameters;

import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.RpcResult;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
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

	@Override
	protected RpcResult sendRPC(String filePath, OtpErlangList searchPath)
			throws ErlangRpcException, RpcException {
		return managedBackend.rpc("wrangler", "tuple_funpar_eclipse", "siisx",
				filePath, parameters.getStartLine(), parameters
						.getStartColumn(), newName, searchPath);
	}

}
