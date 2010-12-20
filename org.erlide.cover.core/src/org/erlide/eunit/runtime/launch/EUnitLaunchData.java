package org.erlide.eunit.runtime.launch;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.erlide.core.ErlangPlugin;
import org.erlide.cover.runtime.launch.LaunchType;

public class EUnitLaunchData {
	
	private LaunchType type;
	private String project;
	private String module;
	private String file;
	private String appProject;
	private String application;
	private boolean cover;
	
	public EUnitLaunchData(ILaunchConfiguration config)
			throws CoreException {
		
		type = LaunchType.valueOf(config.getAttribute(IErlTestAttributes.TYPE,
				LaunchType.MODULE.toString()));
		
		project = config.getAttribute(IErlTestAttributes.PROJECT, "");
		module = config.getAttribute(IErlTestAttributes.MODULE, "");
		file = config.getAttribute(IErlTestAttributes.FILE, "");
		appProject = config.getAttribute(IErlTestAttributes.APP_PROJECT, "");
		cover = Boolean.parseBoolean(config.getAttribute(IErlTestAttributes.APP_PROJECT, Boolean.toString(true)));
		
	}
	
	public LaunchType getType() {
		return type;
	}
	
	public String getProject() {
		return project;
	}
	
	public String getModule() {
		return module;
	}
	
	public String getFile() {
		return file;
	}
	
	public String getAppProject() {
		return appProject;
	}
	
	public String getApp () {
		return application;
	}
	
	public boolean ifCover() {
		return cover;
	}

}
