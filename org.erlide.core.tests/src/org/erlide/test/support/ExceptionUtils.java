/**
 * Original code from:
 * 		http://www.dzone.com/links/r/some_junit_tricks_for_easier_and_better_test_cases.html
 */
package org.erlide.test.support;

import static com.google.common.collect.Lists.*;
import static com.google.common.collect.Sets.*;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.List;
import java.util.Set;

public class ExceptionUtils {
    private static final String INDENT = "\t";
    private static List<String> _suppressedPackages = newArrayList("$Proxy",
            "org.junit", "java.lang.reflect.Method", "sun.", "org.eclipse",
            "junit.framework");

    public static String getFilteredStackTrace(final Throwable t) {
        return getFilteredStackTrace(t, true);
    }

    public static String getFilteredStackTrace(final Throwable t,
            final boolean shouldFilter) {
        try {
            final StringWriter sw = new StringWriter();
            final PrintWriter pw = new PrintWriter(sw);
            writeCleanStackTrace(t, pw, shouldFilter);
            return sw.getBuffer().toString();
        } catch (final Exception e) {
            e.printStackTrace();
            return e.toString();
        }
    }

    private static void writeCleanStackTrace(final Throwable t,
            final PrintWriter s, final boolean wantsFilter) {
        s.print("Exception: ");
        printExceptionChain(t, s);
        final Set<String> skippedPackages = newHashSet();
        int skippedLines = 0;
        final boolean shouldFilter = wantsFilter && filtersEnabled();
        for (final StackTraceElement traceElement : getBottomThrowable(t)
                .getStackTrace()) {
            String forbiddenPackageName = null;
            if (shouldFilter) {
                forbiddenPackageName = tryGetForbiddenPackageName(traceElement);
            }

            if (forbiddenPackageName == null) {
                if (skippedPackages.size() > 0) {
                    // 37 lines skipped for [org.h2, org.hibernate, sun.,
                    // java.lang.reflect.Method, $Proxy]
                    s.println(getSkippedPackagesMessage(skippedPackages,
                            skippedLines));
                }
                // at hib.HibExample.test(HibExample.java:18)
                s.println(INDENT + "at " + traceElement);
                skippedPackages.clear();
                skippedLines = 0;
            } else {
                skippedLines++;
                skippedPackages.add(forbiddenPackageName);
            }
        }
        if (skippedLines > 0) {
            s.println(getSkippedPackagesMessage(skippedPackages, skippedLines));
        }
    }

    // 37 lines skipped for [org.h2, org.hibernate, sun.,
    // java.lang.reflect.Method, $Proxy]
    private static String getSkippedPackagesMessage(
            final Set<String> skippedPackages, final int skippedLines) {
        return INDENT + skippedLines + " line" + (skippedLines == 1 ? "" : "s")
                + " skipped for " + skippedPackages;
    }

    private static Throwable getBottomThrowable(Throwable t) {
        while (t.getCause() != null) {
            t = t.getCause();
        }
        return t;
    }

    /**
     * Check configuration to see if filtering is enabled system-wide
     */
    private static boolean filtersEnabled() {
        return true;
    }

    private static void printExceptionChain(final Throwable t,
            final PrintWriter s) {
        s.println(t);
        if (t.getCause() != null) {
            s.print("Caused by: ");
            printExceptionChain(t.getCause(), s);
        }
    }

    /**
     * Checks to see if the class is part of a forbidden package. If so, it
     * returns the package name from the list of suppressed packages that
     * matches, otherwise it returns null.
     */
    private static String tryGetForbiddenPackageName(
            final StackTraceElement traceElement) {
        final String classAndMethod = traceElement.getClassName() + "."
                + traceElement.getMethodName();
        for (final String pkg : _suppressedPackages) {
            if (classAndMethod.startsWith(pkg)) {
                return pkg;
            }
        }
        return null;
    }
}
