package org.erlide.util;

import java.net.InetAddress;
import java.net.UnknownHostException;

import com.google.common.base.Objects;
import com.google.common.base.Strings;

public class HostnameUtils {

    private static final String erlangLongNameFallback = "127.0.0.1";
    private static final String erlangShortNameFallback = "localhost";

    private static String erlangLongName = erlangLongNameFallback;
    private static String erlangShortName = erlangShortNameFallback;

    public static String getErlangHostName(final boolean longName) {
        return longName ? erlangLongName : erlangShortName;
    }

    public static boolean isThisHost(final String host) {
        return Objects.equal(host, getErlangHostName(true))
                || Objects.equal(host, getErlangHostName(false));
    }

    public static String getJavaLongHostName() {
        InetAddress addr;
        try {
            addr = InetAddress.getLocalHost();
            return addr.getCanonicalHostName();
        } catch (final UnknownHostException e1) {
            ErlLogger.warn("Could not retrieve long host name, " + "defaulting to "
                    + erlangLongNameFallback);
            return erlangLongNameFallback;
        }
    }

    public static String getJavaShortHostName() {
        InetAddress addr;
        try {
            addr = InetAddress.getLocalHost();
            return addr.getHostName();
        } catch (final UnknownHostException e1) {
            ErlLogger.warn("Could not retrieve short host name, " + "defaulting to "
                    + erlangShortNameFallback);
            return erlangShortNameFallback;
        }
    }

    /**
     * Start erlang nodes and find out how they resolve the long/short host
     * names.
     */
    public static void detectHostNames(final String otpHome) {
        final ErlangHostnameRetriever retriever = new ErlangHostnameRetriever(otpHome);
        final String forcedLongName = System.getProperty("erlide.long.name");
        if (!Strings.isNullOrEmpty(forcedLongName)) {
            erlangLongName = forcedLongName;
        } else {
            erlangLongName = retriever.checkHostName(true);
            if (erlangLongName == null) {
                erlangLongName = retriever.checkHostName(true, getJavaLongHostName());
            }
        }
        final String forcedShortName = System.getProperty("erlide.short.name");
        if (!Strings.isNullOrEmpty(forcedShortName)) {
            erlangShortName = forcedShortName;
        } else {
            erlangShortName = retriever.checkHostName(false);
            if (erlangShortName == null) {
                erlangShortName = retriever.checkHostName(false, getJavaShortHostName());
            }
        }
        ErlLogger.debug("Detected:: %s%s && %s%s", erlangShortName,
                forcedShortName != null ? "(forced)" : "", erlangLongName,
                forcedLongName != null ? "(forced)" : "");
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
