package org.erlide.wrangler.refactoring.backend;

import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.wrangler.refactoring.backend.internal.AbstractRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.internal.RefactoringRpcMessage;

public class WranglerRefactoringBackend implements IWranglerBackend {
	static String MODULE = "wrangler";
	static String RENAME_FUNCTION = "rename_fun_eclipse";

	protected Backend backend;

	public WranglerRefactoringBackend(Backend backend) {
		this.backend = backend;
	}

	public IRpcMessage callWithParser(IRpcMessage parser, String functionName,
			String signature, Object... parameters) {
		RpcResult res = backend.call_noexception(MODULE, functionName,
				signature, parameters);
		parser.parse(res);
		return parser;
	}

	public AbstractRefactoringRpcMessage call(String functionName,
			String signature, Object... parameters) {
		RpcResult res = backend.call_noexception(MODULE, functionName,
				signature, parameters);
		AbstractRefactoringRpcMessage message = new RefactoringRpcMessage();
		message.parse(res);
		return message;
	}

	public RpcResult callWithoutParser(String functionName, String signature,
			Object... parameters) {
		return backend.call_noexception(MODULE, functionName, signature,
				parameters);
	}
}
