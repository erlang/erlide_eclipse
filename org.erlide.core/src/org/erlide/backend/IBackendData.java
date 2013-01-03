package org.erlide.backend;

import java.util.Collection;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.erlide.core.model.IBeamLocator;
import org.erlide.runtime.InitialCall;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;

public interface IBackendData {

    RuntimeInfo getRuntimeInfo();

    String getCookie();

    void setCookie(final String cookie);

    boolean isManaged();

    void setManaged(final boolean managed);

    boolean isRestartable();

    void setRestartable(boolean restartable);

    boolean useStartShell();

    void setUseStartShell(final boolean shell);

    boolean hasConsole();

    void setConsole(final boolean console);

    boolean isDebug();

    void setDebug(final boolean debug);

    String getRuntimeName();

    void setRuntimeName(final String name);

    String getNodeName();

    void setNodeName(String nodeName);

    boolean isLongName();

    void setLongName(final boolean longname);

    String getExtraArgs();

    void setExtraArgs(final String xtra);

    String getWorkingDir();

    void setWorkingDir(final String dir);

    Map<String, String> getEnv();

    InitialCall getInitialCall();

    int getDebugFlags();

    boolean isTransient();

    void setTransient(final boolean value);

    boolean isInternal();

    void setInternal(final boolean value);

    String[] getCmdLine();

    String getQualifiedNodeName();

    //

    ILaunch getLaunch();

    ILaunchConfiguration asLaunchConfiguration();

    void setBeamLocator(final IBeamLocator beamLocator);

    IBeamLocator getBeamLocator();

    Collection<String> getInterpretedModules();

    void setInterpretedModules(final Collection<String> interpretedModules);

    boolean shouldLoadOnAllNodes();

    void setLoadAllNodes(final boolean load);

    Collection<IProject> getProjects();

}
