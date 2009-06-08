package org.erlide.jinterface.backend;


public interface RuntimeLauncher {

	public void setBackend(Backend backend);

	public void initializeRuntime();

	public void stop();

}
