package org.erlide.utils;

import java.util.List;

import com.google.common.collect.Lists;

public class ListsUtils {
    private static final List<String> EMPTY_LIST = Lists.newArrayList();

    public static String packList(final Iterable<String> strs, final String sep) {
        final StringBuilder result = new StringBuilder();
        for (final String s : strs) {
            result.append(s).append(sep);
        }
        return result.length() > 0 ? result.substring(0,
                result.length() - sep.length()) : "";
    }

    public static String packArray(final String[] strs, final String sep) {
        final StringBuilder result = new StringBuilder();
        for (final String s : strs) {
            result.append(s).append(sep);
        }
        return result.length() > 0 ? result.substring(0,
                result.length() - sep.length()) : "";
    }

    public static String[] unpackArray(final String str, final String sep) {
        return ListsUtils.unpackList(str, sep).toArray(new String[0]);
    }

    public static List<String> unpackList(final String string, final String sep) {
        if (string.length() == 0) {
            return EMPTY_LIST;
        }
        final String[] v = string.split(sep);
        final List<String> result = Lists.newArrayListWithCapacity(v.length);
        for (final String s : v) {
            result.add(s);
        }
        return result;
    }

}
