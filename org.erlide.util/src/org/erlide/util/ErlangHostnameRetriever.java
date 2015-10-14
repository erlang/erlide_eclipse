package org.erlide.util;

import java.io.IOException;
import java.io.InputStream;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.ericsson.otp.erlang.OtpNode;
import com.google.common.base.Strings;
import com.google.common.collect.Lists;

public class ErlangHostnameRetriever {

    final String otpHome;
    AtomicInteger id = new AtomicInteger();

    private final static boolean verbose = !SystemConfiguration
            .hasFeatureEnabled("erlide.ericsson.user")
            && !SystemConfiguration.hasFeatureEnabled("erlide.quiet");

    public ErlangHostnameRetriever(final String otpHome) {
        this.otpHome = otpHome;
    }

    public String getErlangHostName(final boolean longHost) {
        final String nodeName = "foo" + System.currentTimeMillis();
        final ProcessBuilder builder = new ProcessBuilder(
                Lists.newArrayList(otpHome + "/bin/erl", longHost ? "-name" : "-sname",
                        nodeName, "-setcookie", "erlide"));
        String hostName = null;
        try {
            final Process process = builder.start();
            try {
                final StreamListener listener = new StreamListener(
                        process.getInputStream());
                while (listener.isAlive()) {
                    try {
                        listener.join();
                        hostName = listener.getResult();
                        if (verbose) {
                            ErlLogger.debug("Erlang %s hostname: %s",
                                    longHost ? "long" : "short", hostName);
                        }
                    } catch (final InterruptedException e) {
                    }
                }
            } finally {
                process.destroy();
            }
        } catch (final IOException e) {
            ErlLogger.error(e);
        }
        return hostName;
    }

    public boolean canConnect(final String hostName, final boolean longHost) {
        if (Strings.isNullOrEmpty(hostName)) {
            return false;
        }
        final String nodeName = "foo" + id.incrementAndGet();
        final String fullName = nodeName + "@" + hostName;
        final ProcessBuilder builder = new ProcessBuilder(
                Lists.newArrayList(otpHome + "/bin/erl", longHost ? "-name" : "-sname",
                        fullName, "-setcookie", "erlide"));
        try {
            final Process process = builder.start();
            try {
                final StreamListener listener = new StreamListener(
                        process.getInputStream());
                while (listener.getResult() == null) {
                    try {
                        Thread.sleep(50);
                    } catch (final InterruptedException e) {
                        // ignore
                    }
                }

                final OtpNode node = new OtpNode("jtest", "erlide");
                final boolean result = node.ping(fullName, 2000);
                node.close();
                return result;
            } finally {
                process.destroy();
            }
        } catch (final Exception e) {
            ErlLogger.error(e);
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
