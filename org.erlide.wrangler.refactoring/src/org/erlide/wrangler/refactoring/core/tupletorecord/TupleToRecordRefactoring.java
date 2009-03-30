package org.erlide.wrangler.refactoring.core.tupletorecord;

import org.erlide.jinterface.rpc.RpcException;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.wrangler.refactoring.core.RefactoringParameters;
import org.erlide.wrangler.refactoring.core.WranglerRefactoring;

import com.ericsson.otp.erlang.OtpErlangList;

public class TupleToRecordRefactoring extends WranglerRefactoring {

	private String newParametersName;

	public TupleToRecordRefactoring(RefactoringParameters parameters) {
		super(parameters);

	}

	public void setNewParametersName(String str) {
		newParametersName = str;
	}

	@Override
	public String getName() {
		return "Tuple to record";
	}

	@SuppressWarnings("boxing")
	@Override
	protected RpcResult sendRPC(String filePath, OtpErlangList searchPath)
			throws ErlangRpcException, RpcException {
		return managedBackend.call_noexception("wrangler", "tuple_to_record_eclipse",
				"siiiissxi", filePath, parameters.getStartLine(), parameters
						.getStartColumn(), parameters.getEndLine(), parameters
						.getEndColumn(), newName, newParametersName,
				searchPath, parameters.getEditorTabWidth());
	}
}
