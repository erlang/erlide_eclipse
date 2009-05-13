package org.erlide.runtime.backend.internal;

import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.RuntimeLauncher;

public class StandaloneLauncher implements RuntimeLauncher {

	private Backend backend;

	public StandaloneLauncher() {
	}

	public void setBackend(final Backend backend) {
		this.backend = backend;
	}

	public void connect() {
		backend.doConnect(backend.getName());
	}

	public void stop() {
	}

	public void initializeRuntime() {
	}

}
