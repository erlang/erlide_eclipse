package org.erlide.wrangler.refactoring.backend;

import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.wrangler.refactoring.backend.internal.AbstractRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.internal.RefactoringRpcMessage;

/**
 * This class handles the Erlide backends, and holds special ones for Wrangler
 * operations
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class WranglerRefactoringBackend implements IWranglerBackend {
	static String MODULE = "wrangler";
	static String RENAME_FUNCTION = "rename_fun_eclipse";

	protected Backend backend;

	/**
	 * Default constructor
	 * 
	 * @param backend
	 *            Erlide backend
	 */
	public WranglerRefactoringBackend(Backend backend) {
		this.backend = backend;
	}

	/**
	 * Send an RPC, and allow to define a costum parser
	 * 
	 * @param parser
	 *            parser object
	 * @param functionName
	 *            function name in wrangler.erl
	 * @param signature
	 *            parameters signature
	 * @param parameters
	 *            parameters array
	 * @return parsed RPC message
	 */
	public IRpcMessage callWithParser(IRpcMessage parser, String functionName,
			String signature, Object... parameters) {
		RpcResult res = callWithoutParser(functionName, signature, parameters);
		parser.parse(res);
		return parser;
	}

	/**
	 * Send an RPC and parses it with the default parser
	 * 
	 * @param functionName
	 *            function name in wrangler.erl
	 * @param signature
	 *            parameters signature
	 * @param parameters
	 *            parameters in an array
	 * @return parsed RPC message
	 */
	public AbstractRefactoringRpcMessage call(String functionName,
			String signature, Object... parameters) {
		RpcResult res = callWithoutParser(functionName, signature, parameters);
		AbstractRefactoringRpcMessage message = new RefactoringRpcMessage();
		message.parse(res);
		return message;
	}

	/**
	 * Call an RPC without a parser
	 * 
	 * @param functionName
	 *            function name in wrangler.erl
	 * @param signature
	 *            parameters signature
	 * @param parameters
	 *            parameters array
	 * @return raw RPC result
	 */
	public RpcResult callWithoutParser(final String functionName,
			final String signature, final Object... parameters) {
		ErlLogger
				.info("Wrangler call: " + makeLogStr(functionName, parameters));
		return backend.call_noexception(MODULE, functionName, signature,
				parameters);
	}

	public RpcResult callWithoutParser(final int timeout,
			final String functionName, final String signature,
			final Object... parameters) {
		ErlLogger
				.info("Wrangler call: " + makeLogStr(functionName, parameters));
		return backend.call_noexception(timeout, MODULE, functionName,
				signature, parameters);
	}

	protected String makeLogStr(String function, Object[] parameters) {
		String ret = function + "(";
		for (Object o : parameters) {
			ret += o.toString();
			ret += ", ";
		}
		if (ret.endsWith(", "))
			ret = ret.substring(0, ret.length() - 2);
		return ret + ")";
	}
}
