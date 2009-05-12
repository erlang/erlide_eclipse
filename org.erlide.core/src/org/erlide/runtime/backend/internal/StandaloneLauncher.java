package org.erlide.runtime.backend.internal;

import org.erlide.runtime.backend.Backend;

public class StandaloneLauncher implements RuntimeLauncher {

	private Backend backend;

	public StandaloneLauncher() {
	}

	public void setRuntime(final Backend backend) {
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
