package org.erlide.util;

import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.text.MessageFormat;

/**
 * @author Jeeeyul 2011. 11. 1.
 */
public final class DebugStream extends PrintStream {
    private static DebugStream INSTANCE;

    public static void activate() {
        if (INSTANCE == null) {
            try {
                final DebugStream stream = new DebugStream();
                System.setOut(stream);
                INSTANCE = stream;
            } catch (final UnsupportedEncodingException e) {
                e.printStackTrace();
            }
        }
    }

    private DebugStream() throws UnsupportedEncodingException {
        super(System.out, true, "UTF-8");
    }

    @Override
    public void println(final Object x) {
        showLocation();
        super.println(x);
    }

    @Override
    public void println(final String x) {
        showLocation();
        super.println(x);
    }

    private void showLocation() {
        StackTraceElement element = new Throwable().getStackTrace()[2];
        if (element.getFileName().equals("InputOutput.java")) {
            element = new Throwable().getStackTrace()[3];
        }
        super.print(MessageFormat.format("({0}:{1, number,#}) : ", element.getFileName(),
                element.getLineNumber()));
    }
}
