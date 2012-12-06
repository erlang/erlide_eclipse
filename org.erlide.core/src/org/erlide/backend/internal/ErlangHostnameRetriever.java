package org.erlide.backend.internal;

import java.io.IOException;
import java.io.InputStream;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.erlide.backend.runtimeinfo.RuntimeInfo;
import org.erlide.jinterface.ErlLogger;

import com.ericsson.otp.erlang.OtpNode;
import com.google.common.collect.Lists;

public class ErlangHostnameRetriever {

    private final RuntimeInfo runtime;
    private String nodeName;

    public ErlangHostnameRetriever(final RuntimeInfo runtime) {
        this.runtime = runtime;
    }

    public String checkHostName(final boolean longHost, String hostName) {
        nodeName = "foo" + System.currentTimeMillis();
        final ProcessBuilder builder = new ProcessBuilder(Lists.newArrayList(
                runtime.getOtpHome() + "/bin/erl", longHost ? "-name"
                        : "-sname", nodeName, "-setcookie", "erlide"));
        String result = null;
        try {
            final Process process = builder.start();
            final StreamListener listener = new StreamListener(
                    process.getInputStream());
            while (listener.isAlive()) {
                try {
                    listener.join();
                    if (hostName == null) {
                        hostName = listener.getResult();
                    }
                    ErlLogger.debug("Test %s hostname: %s", longHost ? "long"
                            : "short", hostName);
                    final boolean canConnect = canConnect(hostName);
                    if (canConnect) {
                        result = hostName;
                        ErlLogger.debug("OK");
                    } else {
                        ErlLogger.warn("Can't use %s as %s name", hostName,
                                longHost ? "long" : "short");
                    }
                    process.destroy();
                } catch (final InterruptedException e) {
                }
            }
        } catch (final IOException e) {
            ErlLogger.error(e);
        }
        return result;
    }

    public String checkHostName(final boolean longHost) {
        return checkHostName(longHost, null);
    }

    private boolean canConnect(final String hostName) {
        try {
            final OtpNode node = new OtpNode("jtest", "erlide");
            System.out.println("Ping: " + nodeName + "@" + hostName);
            final boolean result = node.ping(nodeName + "@" + hostName, 1000);
            node.close();
            return result;
        } catch (final IOException e) {
            e.printStackTrace();
        }
        return false;
    }

    private static class StreamListener extends Thread {

        private static Pattern pattern = Pattern
                .compile("^\\([^@]+@([^\\)]+)\\)[0-9]+>.*$");

        private final InputStream stream;
        private String result;

        StreamListener(final InputStream stream) {
            this.stream = stream;
            start();
        }

        @Override
        public void run() {
            final StringBuilder line = new StringBuilder();
            try {
                int chr;
                while ((chr = stream.read()) != -1) {
                    if (chr == 10 || chr == 13) {
                        line.setLength(0);
                    } else {
                        line.append((char) chr);
                    }
                    System.out.println(">> " + line);
                    final Matcher matcher = pattern.matcher(line);
                    if (matcher.matches()) {
                        result = matcher.group(1);
                        return;
                    }
                }
            } catch (final IOException e) {
                ErlLogger.error(e);
            }
        }

        public String getResult() {
            return result;
        }
    }

}
