package org.erlide.runtime;

import java.net.InetAddress;
import java.net.UnknownHostException;

import org.erlide.runtime.internal.ErlangHostnameRetriever;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.utils.ErlLogger;

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
    public static void detectHostNames(final RuntimeInfo runtime) {
        final ErlangHostnameRetriever retriever = new ErlangHostnameRetriever(
                runtime);
        if (runtime != null) {
            erlangLongName = retriever.checkHostName(true);
            if (erlangLongName == null) {
                erlangLongName = retriever.checkHostName(true,
                        getJavaLongHostName());
            }
            erlangShortName = retriever.checkHostName(false);
            if (erlangShortName == null) {
                erlangShortName = retriever.checkHostName(false,
                        getJavaShortHostName());
            }
        }
    }

    public static String getErlangLongHostName() {
        return erlangLongName;
    }

    public static String getErlangShortHostName() {
        return erlangShortName;
    }

    public static boolean canUseLongNames() {
        return erlangLongName != null;
    }

    public static boolean canUseShortNames() {
        return erlangShortName != null;
    }

}
