/*
 * Code borrowed from PyDev
 */
/*
 * @author Fabio Zadrozny Created: June 2005 License: Common Public License v1.0
 */

package org.erlide.util;

import java.util.ArrayList;
import java.util.List;

import com.google.common.collect.Lists;

public final class StringUtils {

    private StringUtils() {
    }

    /**
     * Given a string remove all from the rightmost '.' onwards.
     *
     * E.g.: bbb.t would return bbb
     *
     * If it has no '.', returns the original string unchanged.
     */
    public static String stripExtension(final String input) {
        return StringUtils.stripFromRigthCharOnwards(input, '.');
    }

    public static int rFind(final String input, final char ch) {
        int len = input.length();
        final int st = 0;
        final int off = 0;
        final char[] val = input.toCharArray();

        while (len > st && val[off + len - 1] != ch) {
            len--;
        }
        len--;
        return len;
    }

    private static String stripFromRigthCharOnwards(final String input, final char ch) {
        final int len = StringUtils.rFind(input, ch);
        if (len == -1) {
            return input;
        }
        return input.substring(0, len);
    }

    public static String stripFromLastSlash(final String input) {
        return StringUtils.stripFromRigthCharOnwards(input, '/');
    }

    /**
     * Removes the occurrences of the passed char in the beggining of the string.
     */
    public static String rightTrim(final String input, final char charToTrim) {
        int len = input.length();
        final int st = 0;
        final int off = 0;
        final char[] val = input.toCharArray();

        while (len > st && val[off + len - 1] == charToTrim) {
            len--;
        }
        return input.substring(0, len);
    }

    /**
     * Removes the occurrences of the passed char in the end of the string.
     */
    public static String leftTrim(final String input, final char charToTrim) {
        final int len = input.length();
        int off = 0;
        final char[] val = input.toCharArray();

        while (off < len && val[off] == charToTrim) {
            off++;
        }
        return input.substring(off, len);
    }

    /**
     * Splits the given string in a list where each element is a line.
     *
     * @param string
     *            string to be splitted.
     * @return list of strings where each string is a line.
     *
     * @note the new line characters are also added to the returned string.
     */
    public static List<String> splitLines(final String string) {
        final List<String> ret = new ArrayList<>();
        final int len = string.length();

        char c;
        final StringBuilder buf = new StringBuilder();

        for (int i = 0; i < len; i++) {
            c = string.charAt(i);
            buf.append(c);
            if (c == '\r') {
                if (i < len - 1 && string.charAt(i + 1) == '\n') {
                    i++;
                    buf.append('\n');
                }
                ret.add(buf.toString());
                buf.setLength(0);
            }
            if (c == '\n') {
                ret.add(buf.toString());
                buf.setLength(0);
            }
        }
        if (buf.length() != 0) {
            ret.add(buf.toString());
        }
        return ret;
    }

    public static String unquote(final String s) {
        final int length = s.length();
        if (length > 2 && s.charAt(0) == '\'' && s.charAt(length - 1) == '\'') {
            return s.substring(1, length - 1);
        }
        return s;
    }

    public static String quote(final String s) {
        if (s.startsWith("'")) {
            return s;
        }
        return "'" + s + "'";
    }

    public static String getLongestPrefix(final List<String> list) {
        final StringBuilder b = new StringBuilder();
        int i = 0;
        for (;;) {
            final Character c = StringUtils.checkCharAt(i, list);
            if (c == null) {
                break;
            }
            b.append(c);
            i++;
        }
        return b.toString();
    }

    private static Character checkCharAt(final int i, final List<String> list) {
        Character c = null;
        for (final String s : list) {
            try {
                if (c == null) {
                    c = s.charAt(i);
                } else if (c != s.charAt(i)) {
                    return null;
                }
            } catch (final StringIndexOutOfBoundsException e) {
                return null;
            }
        }
        return c;
    }

    public static List<String> removeCommonPrefixes(final List<String> list) {
        if (list.size() <= 1) {
            return list;
        }
        final int prefixLength = StringUtils.getLongestPrefix(list).length();
        if (prefixLength == 0) {
            return list;
        }
        final List<String> result = Lists.newArrayList();
        for (final String s : list) {
            result.add(s.substring(prefixLength));
        }
        return result;
    }

    public static String normalizeSpaces(final String string) {
        return string.replaceAll("[\t\n\r ]+", " ");
    }

}
