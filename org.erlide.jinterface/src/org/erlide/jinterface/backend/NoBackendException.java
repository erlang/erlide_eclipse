package org.erlide.jinterface.backend;


public class NoBackendException extends BackendException {

	private static final long serialVersionUID = 1L;

	public NoBackendException(final Exception e) {
		super(e);
	}

	public NoBackendException() {
		super("The Erlang backend is not running. Functionality is limited.");
	}

}
