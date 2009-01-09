package org.erlide.runtime.backend.internal;

import org.eclipse.debug.core.ILaunch;
import org.erlide.runtime.backend.Backend;

public interface RuntimeLauncher {

	public void setBackend(Backend backend);

	public void initializeRuntime(ILaunch launch);

	public void connect();

	public void stop();

}
