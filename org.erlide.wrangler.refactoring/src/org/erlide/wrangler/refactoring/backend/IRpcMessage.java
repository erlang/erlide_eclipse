package org.erlide.wrangler.refactoring.backend;

import org.erlide.jinterface.rpc.RpcResult;

/**
 * Interface for handling RpcResult objects of the Erlide's backend, which
 * contains information about a RPC with Wrangler
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public interface IRpcMessage {
	/**
	 * Returns true if the wrangler operation was successful.
	 * 
	 * @return true if the wrangler operation node was successful
	 */
	public boolean isSuccessful();

	/**
	 * If Wrangler could not perform the operation, it returns with an error
	 * message. It is returned by this function.
	 * 
	 * @return error message from wrangler
	 */
	public String getMessage();

	/**
	 * Parses an RpcResult object.
	 */
	public void parse(RpcResult result);

}
