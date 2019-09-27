package org.erlide.util;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.xtext.xbase.lib.ListExtensions;

public final class PreferencesUtils {

    private static final String SEP = ";";

    public static String packList(final Iterable<String> list) {
        return ListsUtils.packList(list, PreferencesUtils.SEP);
    }

    public static List<String> unpackList(final String string) {
        List<String> result = ListsUtils.unpackList(string, PreferencesUtils.SEP);
        result = ListExtensions.map(result, p -> p.trim());
        return result;
    }

    public static String packArray(final String[] strs) {
        final StringBuilder result = new StringBuilder();
        for (final String s : strs) {
            if (!s.isEmpty()) {
                result.append(s.trim()).append(PreferencesUtils.SEP);
            }
        }
        final String r = result.length() == 0 ? ""
                : result.substring(0, result.length() - PreferencesUtils.SEP.length());
        return r;
    }

    public static String[] unpackArray(final String str) {
        return PreferencesUtils.unpackList(str).toArray(new String[0]);
    }

    public static List<String> readFile(final String file) {
        final List<String> res = new ArrayList<>();
        try (final BufferedReader reader = new BufferedReader(new FileReader(file))) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (!line.isEmpty()) {
                    res.add(line);
                }
            }
        } catch (final IOException e) {
        }
        return res;
    }

    private PreferencesUtils() {
    }

}
