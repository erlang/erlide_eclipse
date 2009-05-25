package org.erlide.runtime.backend;


public interface IShellManager {

	public abstract IShell get(String id);

	public abstract IShell openShell(String id);

	public abstract void closeShell(String id);

}