package org.erlide.utils;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public final class PreferencesUtils {

    private static final String SEP = ";";

    public static String packList(final Iterable<String> list) {
        return ListsUtils.packList(list, SEP);
    }

    public static List<String> unpackList(final String string) {
        return ListsUtils.unpackList(string, SEP);
    }

    public static String packArray(final String[] strs) {
        final StringBuilder result = new StringBuilder();
        for (final String s : strs) {
            if (s.length() > 0) {
                result.append(s).append(SEP);
            }
        }
        final String r = result.length() == 0 ? "" : result.substring(0,
                result.length() - SEP.length());
        return r;
    }

    public static String[] unpackArray(final String str) {
        return unpackList(str).toArray(new String[0]);
    }

    public static List<String> readFile(final String file) {
        final List<String> res = new ArrayList<String>();
        try {
            final BufferedReader reader = new BufferedReader(new FileReader(
                    file));
            try {
                String line;
                while ((line = reader.readLine()) != null) {
                    if (line.length() > 0) {
                        res.add(line);
                    }
                }
            } finally {
                reader.close();
            }
        } catch (final IOException e) {
        }
        return res;
    }

    private PreferencesUtils() {
    }

}
