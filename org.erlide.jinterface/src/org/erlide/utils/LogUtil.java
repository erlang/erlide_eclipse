package org.erlide.utils;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.eclipse.core.runtime.Platform;
import org.erlide.jinterface.ErlLogger;

public class LogUtil {

    public static String getReportFile() {
        final String s = getReportLocation();
        final String tstamp = new SimpleDateFormat("yyyyMMdd_HHmmss")
                .format(new Date());
        return s + "/" + System.getProperty("user.name") + "_" + tstamp
                + ".txt";
    }

    public static String getReportLocation() {
        String s = System.getProperty("erlide.logDirectory");
        if (s == null) {
            if (SystemUtils.getInstance().isOnWindows()) {
                s = "\\\\vhub\\tecsas\\shade\\erlide\\reports";
            } else {
                s = "/proj/tecsas/SHADE/erlide/reports";
            }
        }
        final File dir = new File(s);
        if (!dir.exists()) {
            s = System.getProperty("user.home");
        }
        return s;
    }

    public static String fetchErlideLog() {
        final StringBuffer result = new StringBuffer();
        final File log = new File(ErlLogger.getInstance().getLogLocation());
        try {
            final BufferedReader reader = new BufferedReader(
                    new InputStreamReader(new FileInputStream(log), "UTF-8"));
            try {
                for (;;) {
                    String line = reader.readLine();
                    if (line == null) {
                        break;
                    }
                    line = line.trim();
                    if (line.length() == 0) {
                        continue;
                    }
                    result.append(line).append('\n');
                }
            } finally {
                reader.close();
            }
        } catch (final Exception e) {
            ErlLogger.warn(e);
        }
        return result.toString();
    }

    public static String fetchPlatformLog() {
        final List<String> result = new ArrayList<String>();
        final File log = Platform.getLogFileLocation().toFile();
        try {
            final BufferedReader reader = new BufferedReader(
                    new InputStreamReader(new FileInputStream(log), "UTF-8"));
            try {
                for (;;) {
                    String line = reader.readLine();
                    if (line == null) {
                        break;
                    }
                    line = line.trim();
                    if (line.length() == 0) {
                        continue;
                    }
                    if (line.startsWith("!SESSION ")) {
                        result.clear();
                    }
                    result.add(line);
                }
            } finally {
                reader.close();
            }
        } catch (final Exception e) {
            ErlLogger.warn(e);
        }
        final StringBuffer buf = new StringBuffer();
        for (final String s : result) {
            buf.append(s).append('\n');
        }
        return buf.toString();
    }

    public static String fetchStraceLog(final String filename) {
        final StringBuffer result = new StringBuffer();
        final File log = new File(filename);
        if (log.exists()) {
            try {
                final BufferedReader reader = new BufferedReader(
                        new InputStreamReader(new FileInputStream(log), "UTF-8"));
                try {
                    for (;;) {
                        String line = reader.readLine();
                        if (line == null) {
                            break;
                        }
                        line = line.trim();
                        if (line.length() == 0) {
                            continue;
                        }
                        result.append(line).append('\n');
                    }
                } finally {
                    reader.close();
                }
            } catch (final Exception e) {
            }
        }
        return result.toString();
    }

    private LogUtil() {
    }

}
