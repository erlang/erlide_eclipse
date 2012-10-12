package org.erlide.backend;

import java.net.InetAddress;
import java.net.UnknownHostException;

import org.erlide.backend.internal.ErlangHostnameRetriever;
import org.erlide.backend.runtimeinfo.RuntimeInfo;
import org.erlide.jinterface.ErlLogger;

import com.google.common.collect.Lists;

public class HostnameUtils {

    private static final String erlangLongNameFallback = "127.0.0.1";
    private static final String erlangShortNameFallback = "localhost";

    private static String erlangLongName = erlangLongNameFallback;
    private static String erlangShortName = erlangShortNameFallback;

    public static String getErlangHostName(final boolean longName) {
        return longName ? erlangLongName : erlangShortName;
    }

    public static boolean isThisHost(final String host) {
        return getErlangHostName(true).equals(host)
                || getErlangHostName(false).equals(host);
    }

    public static String getJavaLongHostName() {
        InetAddress addr;
        try {
            addr = InetAddress.getLocalHost();
            return addr.getCanonicalHostName();
        } catch (final UnknownHostException e1) {
            ErlLogger.warn("Could not retrieve long host name, "
                    + "defaulting to " + erlangLongNameFallback);
            return erlangLongNameFallback;
        }
    }

    public static String getJavaShortHostName() {
        InetAddress addr;
        try {
            addr = InetAddress.getLocalHost();
            return addr.getHostName();
        } catch (final UnknownHostException e1) {
            ErlLogger.warn("Could not retrieve short host name, "
                    + "defaulting to " + erlangShortNameFallback);
            return erlangShortNameFallback;
        }
    }

    /**
     * Start erlang nodes and find out how they resolve the long/short host
     * names.
     */
    public static void detectHostNames() {
        final RuntimeInfo runtime = BackendCore.getRuntimeInfoManager()
                .getErlideRuntime();
        detectHostNames(runtime);
    }

    public static void detectHostNames(final RuntimeInfo runtime) {
        final ErlangHostnameRetriever retriever = new ErlangHostnameRetriever();
        if (runtime != null) {
            final String nname = "foo";
            erlangLongName = retriever.checkHostName(Lists.newArrayList(
                    runtime.getOtpHome() + "/bin/erl", "-name",
                    nname + System.currentTimeMillis()));
            erlangShortName = retriever.checkHostName(Lists.newArrayList(
                    runtime.getOtpHome() + "/bin/erl", "-sname",
                    nname + System.currentTimeMillis()));
        } else {
            erlangLongName = getJavaLongHostName();
            erlangShortName = getJavaShortHostName();
        }
    }

    public static String getErlangLongHostName() {
        return erlangLongName;
    }

    public static String getErlangShortHostName() {
        return erlangShortName;
    }

    public static boolean canUseLongNames() {
        return erlangLongName != null && erlangLongName.contains(".");
    }

    public static boolean canUseShortNames() {
        boolean resolvable;
        try {
            final InetAddress addr = InetAddress.getByName(erlangShortName);
            resolvable = addr != null;
        } catch (final UnknownHostException e) {
            resolvable = false;
        }
        return erlangShortName != null && resolvable;
    }

}
