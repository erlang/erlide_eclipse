package org.erlide.runtime.backend.exceptions;

import org.erlide.jinterface.rpc.RpcException;

public class NoBackendException extends RpcException {

	private static final long serialVersionUID = 1L;

	public NoBackendException(Exception e) {
		super(e);
	}

	public NoBackendException() {
		super("The Erlang backend is not running. Functionality is limited.");
	}

}
