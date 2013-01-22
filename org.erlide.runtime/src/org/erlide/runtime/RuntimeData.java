package org.erlide.runtime;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.jdt.annotation.Nullable;
import org.erlide.runtime.epmd.EpmdWatcher;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.utils.Asserts;
import org.erlide.utils.ErlLogger;
import org.erlide.utils.SystemConfiguration;

import com.google.common.base.Strings;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public class RuntimeData {
    protected String cookie;
    protected boolean managed;
    protected boolean restartable;
    protected boolean startShell;
    protected boolean console;
    protected List<String> interpretedModules;
    protected String runtimeName;
    protected String nodeName;
    protected boolean longName;
    protected String extraArgs;
    protected String workingDir;
    protected Map<String, String> env;
    protected InitialCall initialCall;
    protected EnumSet<ErlDebugFlags> debugFlags;
    protected boolean loadOnAllNodes;
    protected boolean internal;
    protected RuntimeInfo runtimeInfo;
    protected boolean debug;
    protected boolean reportErrors = false;

    public RuntimeData() {
        cookie = "";
        managed = true;
        restartable = false;
        startShell = true;
        console = true;
        interpretedModules = Lists.newArrayList();
        runtimeName = "";
        nodeName = "";
        longName = true;
        extraArgs = "";
        workingDir = ".";
        env = Maps.newHashMap();
        initialCall = null;
        debugFlags = ErlDebugFlags.DEFAULT_DEBUG_FLAGS;
        loadOnAllNodes = false;
        internal = false;
        interpretedModules = Lists.newArrayList();
        runtimeInfo = null;
        debug = false;
    }

    public RuntimeData(final RuntimeInfo runtime, final String mode) {
        this();
        runtimeInfo = runtime;
        debug = mode.equals("debug");
    }

    public RuntimeData(final RuntimeInfo info, final String mode,
            final String defaultWorkingDir) {
        this(info, mode);
        Asserts.isNotNull(info, "Can't create backend with no runtime info");

        runtimeInfo = info;
        setRuntimeName(info.getName());
        setCookie("erlide");
        setLongName(true);

        setWorkingDir(defaultWorkingDir);
        setExtraArgs(info.getArgs());

        setConsole(true);
        setLoadAllNodes(false);
    }

    public String getCookie() {
        return cookie;
    }

    public void setCookie(final String cookie) {
        this.cookie = cookie.trim();
    }

    public boolean isManaged() {
        return managed;
    }

    public void setManaged(final boolean managed) {
        this.managed = managed;
    }

    public boolean isRestartable() {
        return restartable;
    }

    public void setRestartable(final boolean restartable) {
        this.restartable = restartable;
    }

    public boolean useStartShell() {
        return startShell;
    }

    public void setUseStartShell(final boolean shell) {
        startShell = shell;
    }

    public boolean hasConsole() {
        return console;
    }

    public void setConsole(final boolean console) {
        this.console = console;
    }

    public boolean isDebug() {
        return debug;
    }

    public void setDebug(final boolean debug) {
        this.debug = debug;
    }

    public List<String> getInterpretedModules() {
        return interpretedModules;
    }

    public void setInterpretedModules(final List<String> interpretedModules) {
        this.interpretedModules = interpretedModules;
    }

    public String getRuntimeName() {
        return runtimeName;
    }

    public void setRuntimeName(final String name) {
        runtimeName = name;
    }

    public String getNodeName() {
        return nodeName;
    }

    public void setNodeName(String nodeName) {
        if (!validateNodeName(nodeName)) {
            // TODO this still can create a name that isn't valid
            nodeName = nodeName.replaceAll("[^a-zA-Z0-9_-]", "");
        }
        this.nodeName = nodeName;
    }

    public static boolean validateNodeName(final String name) {
        return name != null
                && name.matches("[a-zA-Z0-9_-]+(@[a-zA-Z0-9_.-]+)?");
    }

    public boolean hasLongName() {
        return longName;
    }

    public void setLongName(final boolean longname) {
        longName = longname;
    }

    public String getExtraArgs() {
        return extraArgs;
    }

    public void setExtraArgs(final String xtra) {
        extraArgs = xtra;
    }

    public String getWorkingDir() {
        return workingDir;
    }

    public void setWorkingDir(final String dir) {
        workingDir = dir;
    }

    public Map<String, String> getEnv() {
        return env;
    }

    @Nullable
    public InitialCall getInitialCall() {
        return initialCall;
    }

    public RuntimeInfo getRuntimeInfo() {
        return runtimeInfo;
    }

    public EnumSet<ErlDebugFlags> getDebugFlags() {
        return debugFlags;
    }

    public boolean shouldLoadOnAllNodes() {
        return loadOnAllNodes;
    }

    public void setLoadAllNodes(final boolean load) {
        loadOnAllNodes = load;
    }

    public boolean isInternal() {
        return internal;
    }

    public void setInternal(final boolean internal) {
        this.internal = internal;
    }

    public String[] getCmdLine() {
        final RuntimeInfo r = getRuntimeInfo();
        final List<String> result = new ArrayList<String>();

        if (hasDetachedConsole() && !isInternal()) {
            if (SystemConfiguration.getInstance().isOnWindows()) {
                result.add("cmd.exe");
                result.add("/c");
                result.add("start");
            } else {
                final String command = System.getenv().get("TERM");
                result.add(command);
                result.add("-e");
            }
        }

        String erl = r.getOtpHome() + "/bin/erl";
        if (erl.indexOf(' ') >= 0) {
            erl = "\"" + erl + "\"";
        }
        result.add(erl);
        for (final String path : r.getCodePath()) {
            if (!Strings.isNullOrEmpty(path)) {
                result.add("-pa");
                result.add(path);
            }
        }
        if (!useStartShell()) {
            result.add("-noshell");
        }

        if (!getNodeName().equals("")) {
            final String nameTag = hasLongName() ? "-name" : "-sname";
            String nameOption = getNodeName();
            if (!nameOption.contains("@")) {
                nameOption += "@"
                        + HostnameUtils.getErlangHostName(hasLongName());
            }
            result.add(nameTag);
            result.add(nameOption);
            final String cky = getCookie();
            if (!Strings.isNullOrEmpty(cky)) {
                result.add("-setcookie");
                result.add(cky);
            }
        }
        final String gotArgs = r.getArgs();
        if (!Strings.isNullOrEmpty(gotArgs)) {
            result.addAll(splitQuoted(gotArgs));
        }
        return result.toArray(new String[result.size()]);
    }

    private boolean hasDetachedConsole() {
        // TODO add GUI for "detached console"
        return "true".equals(System.getProperty("erlide.backend.detached"));
    }

    /**
     * split on spaces but respect quotes
     * 
     * @param theArgs
     * @return
     */
    private Collection<String> splitQuoted(final String theArgs) {
        final Pattern p = Pattern.compile("(\"[^\"]*?\"|'[^']*?'|\\S+)");
        final Matcher m = p.matcher(theArgs);
        final List<String> tokens = new ArrayList<String>();
        while (m.find()) {
            tokens.add(m.group(1));
        }
        return tokens;
    }

    public String getQualifiedNodeName() {
        final String erlangHostName = HostnameUtils
                .getErlangHostName(hasLongName());
        final String name = getNodeName();
        final boolean hasHost = name.contains("@");
        return hasHost ? name : name + "@" + erlangHostName;
    }

    public boolean isReportErrors() {
        return reportErrors;
    }

    public void setReportErrors(final boolean value) {
        reportErrors = value;
    }

    public void debugPrint() {
        ErlLogger.info("Data:: " + getClass().getName());
        for (final Field field : getAllPrivateFields(getClass())) {
            try {
                final boolean access = field.isAccessible();
                field.setAccessible(true);
                try {
                    ErlLogger.info("%-20s: %s", field.getName(),
                            field.get(this));
                } finally {
                    field.setAccessible(access);
                }
            } catch (final Exception e) {
                ErlLogger.info("Could not read %s! %s", field.getName(),
                        e.getMessage());
            }
        }
        ErlLogger.info("---------------");
    }

    protected List<Field> getAllPrivateFields(final Class<?> type) {
        final List<Field> result = new ArrayList<Field>();

        Class<?> cls = type;
        while (cls != null && cls != Object.class) {
            for (final Field field : cls.getDeclaredFields()) {
                if (!field.isSynthetic()) {
                    result.add(field);
                }
            }
            cls = cls.getSuperclass();
        }

        return result;
    }

    public static boolean shouldManageNode(final String name,
            final EpmdWatcher epmdWatcher) {
        final int atSignIndex = name.indexOf('@');
        String shortName = name;
        if (atSignIndex > 0) {
            shortName = name.substring(0, atSignIndex);
        }

        boolean isLocal = atSignIndex < 0;
        if (atSignIndex > 0) {
            final String hostname = name.substring(atSignIndex + 1);
            if (HostnameUtils.isThisHost(hostname)) {
                isLocal = true;
            }
        }

        final boolean isRunning = epmdWatcher.hasLocalNode(shortName);
        final boolean result = isLocal && !isRunning;
        return result;
    }

    public boolean getReportErrors() {
        return reportErrors;
    }

}
