package org.erlide.backend;


public interface RuntimeLauncher {

	public void setRuntime(Backend backend);

	public void initializeRuntime();

	public void connect();

	public void stop();

}
