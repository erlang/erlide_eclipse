package org.erlide.runtime.backend.internal;

import org.eclipse.debug.core.ILaunch;
import org.erlide.runtime.backend.Backend;

public class StandaloneLauncher implements RuntimeLauncher {

	private Backend backend;
	private ILaunch launch;

	public StandaloneLauncher(ILaunch aLaunch) {
		launch = aLaunch;
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
