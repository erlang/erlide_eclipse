package org.erlide.backend;


public interface RuntimeLauncher {

	public void setBackend(Backend backend);

	public void initializeRuntime();

	public void connect();

	public void stop();

}
