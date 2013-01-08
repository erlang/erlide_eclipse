package org.erlide.runtime;

import java.util.List;
import java.util.Map;

import org.erlide.runtime.runtimeinfo.RuntimeInfo;

public interface IRuntimeData {
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

    boolean isReportErrors();

    void setReportErrors(final boolean value);

    boolean isInternal();

    void setInternal(final boolean value);

    String[] getCmdLine();

    String getQualifiedNodeName();

    boolean shouldLoadOnAllNodes();

    void setLoadAllNodes(final boolean load);

    List<String> getInterpretedModules();

    void setInterpretedModules(List<String> interpretedModules);

}
