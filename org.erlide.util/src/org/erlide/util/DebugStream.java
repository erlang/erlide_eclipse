package org.erlide.util;

import java.io.PrintStream;
import java.text.MessageFormat;

/**
 * @author Jeeeyul 2011. 11. 1.
 */
public final class DebugStream extends PrintStream {
    private static final DebugStream INSTANCE = new DebugStream();

    public static void activate() {
        System.setOut(INSTANCE);
    }

    private DebugStream() {
        super(System.out);
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
        final StackTraceElement element = new Throwable().getStackTrace()[2];
        super.print(MessageFormat.format("({0}:{1, number,#}) : ",
                element.getFileName(), element.getLineNumber()));
    }
}
