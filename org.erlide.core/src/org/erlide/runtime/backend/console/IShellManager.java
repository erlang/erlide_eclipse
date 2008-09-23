package org.erlide.runtime.backend.console;

public interface IShellManager {

	public abstract BackendShell get(String id);

	public abstract BackendShell openShell(String id);

	public abstract void closeShell(String id);

}