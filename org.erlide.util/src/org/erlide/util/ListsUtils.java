package org.erlide.util;

import java.util.List;

import com.google.common.base.Joiner;
import com.google.common.base.Splitter;
import com.google.common.collect.Lists;

public class ListsUtils {
    public static String packList(final Iterable<String> strs, final String sep) {
        return Joiner.on(sep).join(strs);
    }

    public static String packArray(final String[] strs, final String sep) {
        return Joiner.on(sep).join(strs);
    }

    public static String[] unpackArray(final String str, final String sep) {
        return unpackList(str, sep).toArray(new String[0]);
    }

    public static List<String> unpackList(final String string, final String sep) {
        if (string.length() == 0) {
            return Lists.newArrayList();
        }
        return Lists.newArrayList(Splitter.on(sep).split(string));
    }

}
