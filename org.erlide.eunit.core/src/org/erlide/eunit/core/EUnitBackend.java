package org.erlide.eunit.core;

import java.io.File;
import java.util.EnumSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.eunit.runtime.launch.EUnitLaunchData;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.RuntimeInfo;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.BackendManager.BackendOptions;
import org.erlide.runtime.backend.ErtsProcess;
import org.erlide.runtime.launch.ErlLaunchAttributes;
import org.erlide.runtime.launch.ErlLaunchData;



public class EUnitBackend {
	
	public static EUnitBackend instance;
	
	public static final String NODE_NAME = "cover_node1"; //
	
	private Backend backend;
	private RuntimeInfo info;
	private ILaunchConfiguration config;
	private EUnitEventHandler handler;
	
	private EUnitLaunchData eunitData;
	private String type;
	private String nodeName;
	//private List<IEUnitObserver> listeners = new LinkedList<IEUnitObserver>();

	public static EUnitBackend getInstance(){
		if(instance == null)
			instance = new EUnitBackend();
		return instance;
	}
	
	private EUnitBackend() {
		handler = new EUnitEventHandler();
	}
	
	public void initialize(ErlLaunchData data, EUnitLaunchData eunitData) 
			throws RuntimeException, BackendException {
		
		final RuntimeInfo rt0 = ErlangCore.getRuntimeInfoManager().getRuntime(
                data.runtime);
       
		if (rt0 == null) {
            ErlLogger.error("Could not find runtime %s", data.runtime);
            throw new RuntimeException();
        }
        		
        this.eunitData = eunitData;
        
        switch(eunitData.getType()) {
        case ALL: type = "all"; break;
        case APPLICATION: type = "application"; break;
        case MODULE: type = "module"; break;
        }
        
        ErlLogger.debug("Backend created...");
        System.out.println("Create backend");
                
		this.info = buildRuntimeInfo(data, rt0);
		EnumSet<BackendOptions> options = EnumSet.of(BackendOptions.AUTOSTART, BackendOptions.NO_CONSOLE);
		this.config = getLaunchConfiguration(info, options);
		
		this.backend = createBackend();
		
		backend.getEventDaemon().addHandler(handler);
		
	}
	

	
	
	public void start() {
		
		String path;
		String pName = eunitData.getProject();
	
		IProject p = ResourcesPlugin.getWorkspace().getRoot()
        .getProject(pName);
		
		if(p != null) {
			 path = p.getLocation().toString() + "/src";
			
		//	ErlLogger.debug(path);
			
		//	IErlProject erlProj = ErlangCore.getModel().getErlangProject(pName);
		    
			ErlLogger.debug("Starting cover ..");
			
			System.out.println("Starting cover..");
			
			try {
				backend.cast(Constants.ERLANG_HELPER, Constants.FUN_START, "sss",
						type , eunitData.getModule(), path);
				System.out.println("Cast sent");
			} catch (BackendException e) {
				e.printStackTrace();
				//TODO: throw exception or show a dialog - not started
			}
		}
	
	}
	
	public EUnitEventHandler getHandler() {
		return handler;
	}
	
	public Backend getBackend(){
		return backend;
	}
	
	public void addListener(IEUnitObserver listener) {
		handler.addListener(listener);
	}
	
	public List<IEUnitObserver> getListeners(){
		return handler.getListeners();
	}
	
	private Backend createBackend() throws BackendException{
	    if (info != null) {
	        try {
	            info.setNodeName(NODE_NAME);
	            info.setStartShell(false);
	            
	            ErlLogger.debug("launching....");
	            System.out.println("Creating Backend");
	            
	            config.launch(ILaunchManager.RUN_MODE, new NullProgressMonitor(), false, false);
	            
	            System.out.println("after launching");
	            
	            return BackendManager.getDefault().getByName(nodeName);
	        } catch (Exception e) {
	            ErlLogger.error(e);
	            e.printStackTrace();
	            throw new BackendException(e);
	        }
	    }
	    throw new BackendException();
	}
	
	
	private RuntimeInfo buildRuntimeInfo(final ErlLaunchData data,
			final RuntimeInfo rt0) {
        final RuntimeInfo rt = RuntimeInfo.copy(rt0, false);
        rt.setNodeName(data.nodeName);
        rt.setCookie(data.cookie);

        rt.setStartShell(true);
        final File d = new File(data.workingDir);
        if (d.isAbsolute()) {
            rt.setWorkingDir(data.workingDir);
        } else {
            final String wspace = ResourcesPlugin.getWorkspace().getRoot()
                    .getLocation().toPortableString();
            rt.setWorkingDir(wspace + "/" + data.workingDir);
        }
        rt.setArgs(data.xtraArgs);
        rt.useLongName(data.longName);
        rt.hasConsole(data.console);
        rt.setLoadAllNodes(data.loadAllNodes);
        
        System.out.println("runtimeInfo build");
        
        return rt;
    }
	
	private ILaunchConfiguration getLaunchConfiguration(RuntimeInfo info, Set<BackendOptions> options) {
        ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
        ILaunchConfigurationType type = manager.getLaunchConfigurationType(ErtsProcess.CONFIGURATION_TYPE_INTERNAL);
        ILaunchConfigurationWorkingCopy workingCopy;
        
        nodeName = info.getNodeName();
        try {
            workingCopy = type.newInstance(null, "internal " + info.getNodeName());
            workingCopy.setAttribute(DebugPlugin.ATTR_CONSOLE_ENCODING, "ISO-8859-1");
            workingCopy.setAttribute(ErlLaunchAttributes.NODE_NAME, info.getNodeName());
            workingCopy.setAttribute(ErlLaunchAttributes.RUNTIME_NAME, info.getName());
            workingCopy.setAttribute(ErlLaunchAttributes.COOKIE, info.getCookie());
            workingCopy.setAttribute(ErlLaunchAttributes.CONSOLE, !options.contains(BackendOptions.NO_CONSOLE));
            workingCopy.setAttribute(ErlLaunchAttributes.INTERNAL, options.contains(BackendOptions.INTERNAL));
            workingCopy.setAttribute(ErlLaunchAttributes.USE_LONG_NAME, false);
            return workingCopy.doSave();
        } catch (CoreException e) {
            e.printStackTrace();
            return null;
        }
    }
	

}
