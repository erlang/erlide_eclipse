package org.erlide.backend.internal;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.erlide.jinterface.ErlLogger;

public class ErlangHostnameRetriever {

    public String checkHostName(final List<String> cmdLine) {
        final ProcessBuilder builder = new ProcessBuilder(cmdLine);
        try {
            final Process process = builder.start();
            final StreamListener listener = new StreamListener(
                    process.getInputStream());
            while (listener.isAlive()) {
                try {
                    listener.join();
                    process.destroy();
                    return listener.getResult();
                } catch (final InterruptedException e) {
                }
            }
        } catch (final IOException e) {
            ErlLogger.error(e);
        }
        return null;
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
