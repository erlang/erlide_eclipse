package org.erlide.backend;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.erlide.core.model.IBeamLocator;
import org.erlide.runtime.InitialCall;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;

public interface IBackendData {

    public static final String PROJECT_NAME_SEPARATOR = ";";

    public abstract RuntimeInfo getRuntimeInfo();

    public abstract ILaunch getLaunch();

    public abstract ILaunchConfiguration asLaunchConfiguration();

    public abstract String getCookie();

    public abstract void setCookie(final String cookie);

    public abstract boolean isManaged();

    public abstract void setManaged(final boolean managed);

    public abstract boolean isAutostart();

    public abstract void setAutostart(final boolean autostart);

    public abstract boolean useStartShell();

    public abstract void setUseStartShell(final boolean shell);

    public abstract boolean hasConsole();

    public abstract void setConsole(final boolean console);

    public abstract boolean isDebug();

    public abstract void setDebug(final boolean debug);

    public abstract Collection<String> getInterpretedModules();

    public abstract void setInterpretedModules(
            final Collection<String> interpretedModules);

    public abstract String getRuntimeName();

    public abstract void setRuntimeName(final String name);

    public abstract String getNodeName();

    public abstract void setNodeName(String nodeName);

    public abstract boolean isLongName();

    public abstract void setLongName(final boolean longname);

    public abstract String getExtraArgs();

    public abstract void setExtraArgs(final String xtra);

    public abstract String getWorkingDir();

    public abstract void setWorkingDir(final String dir);

    public abstract Map<String, String> getEnv();

    public abstract InitialCall getInitialCall();

    public abstract Collection<IProject> getProjects();

    public abstract int getDebugFlags();

    public abstract boolean isLoadAllNodes();

    public abstract void setLoadAllNodes(final boolean load);

    public abstract void setAttribute(final String key, final List<String> value);

    public abstract void setBeamLocator(final IBeamLocator beamLocator);

    public abstract IBeamLocator getBeamLocator();

    public abstract boolean isTransient();

    public abstract void setTransient(final boolean value);

    public abstract boolean isInternal();

    public abstract void setInternal(final boolean value);

    public abstract String[] getCmdLine();

}
