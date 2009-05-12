package org.erlide.runtime.backend.internal;

import org.erlide.runtime.backend.Backend;

public interface RuntimeLauncher {

	public void setRuntime(Backend backend);

	public void initializeRuntime();

	public void connect();

	public void stop();

}
