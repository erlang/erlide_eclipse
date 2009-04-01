package org.erlide.runtime.backend.exceptions;


public class NoBackendException extends BackendException {

	private static final long serialVersionUID = 1L;

	public NoBackendException(Exception e) {
		super(e);
	}

	public NoBackendException() {
		super("The Erlang backend is not running. Functionality is limited.");
	}

}
