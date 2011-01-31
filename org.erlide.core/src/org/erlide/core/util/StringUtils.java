/*
 * Code borrowed from PyDev
 */
/*
 * @author Fabio Zadrozny
 * Created: June 2005
 * License: Common Public License v1.0
 */

package org.erlide.core.util;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.runtime.Path;

public class StringUtils {

    private StringUtils() {
    }

    public static final Object EMPTY = "";

    public static String joinWithSpaces(final String[] cmds) {
        return join(" ", cmds);
    }

    /**
     * Formats a string, replacing %s with the arguments passed.
     * 
     * @param str
     *            string to be formatted
     * @param args
     *            arguments passed
     * @return a string with the %s replaced by the arguments passed
     */
    public static String format(final String str, final Object... args) {
        final StringBuilder buffer = new StringBuilder(str.length() + 16
                * args.length);
        int j = 0;

        for (int i = 0; i < str.length(); i++) {
            final char c = str.charAt(i);
            if (c == '%' && i + 1 < str.length()) {
                final char nextC = str.charAt(i + 1);
                if (nextC == 's') {
                    buffer.append(args[j].toString());
                    j++;
                    i++;
                } else if (nextC == '%') {
                    buffer.append('%');
                    j++;
                    i++;
                }
            } else {
                buffer.append(c);
            }
        }
        return buffer.toString();
    }

    /**
     * Counts the number of %s in the string
     * 
     * @param str
     *            the string to be analyzide
     * @return the number of %s in the string
     */
    public static int countPercS(final String str) {
        int j = 0;

        final int len = str.length();
        for (int i = 0; i < len; i++) {
            final char c = str.charAt(i);
            if (c == '%' && i + 1 < len) {
                final char nextC = str.charAt(i + 1);
                if (nextC == 's') {
                    j++;
                    i++;
                }
            }
        }
        return j;
    }

    /**
     * Removes whitespaces at the beggining of the string.
     */
    public static String rightTrim(final String input) {
        int len = input.length();
        final int st = 0;
        final int off = 0;
        final char[] val = input.toCharArray();

        while (st < len && val[off + len - 1] <= ' ') {
            len--;
        }
        return input.substring(0, len);
    }

    /**
     * Removes whitespaces at the end of the string.
     */
    public static String leftTrim(final String input) {
        final int len = input.length();
        int off = 0;
        final char[] val = input.toCharArray();

        while (off < len && val[off] <= ' ') {
            off++;
        }
        return input.substring(off, len);
    }

    /**
     * Given a string remove all from the rightmost '.' onwards.
     * 
     * E.g.: bbb.t would return bbb
     * 
     * If it has no '.', returns the original string unchanged.
     */
    public static String stripExtension(final String input) {
        return stripFromRigthCharOnwards(input, '.');
    }

    public static int rFind(final String input, final char ch) {
        int len = input.length();
        final int st = 0;
        final int off = 0;
        final char[] val = input.toCharArray();

        while (st < len && val[off + len - 1] != ch) {
            len--;
        }
        len--;
        return len;
    }

    private static String stripFromRigthCharOnwards(final String input,
            final char ch) {
        final int len = rFind(input, ch);
        if (len == -1) {
            return input;
        }
        return input.substring(0, len);
    }

    public static String stripFromLastSlash(final String input) {
        return stripFromRigthCharOnwards(input, '/');
    }

    /**
     * Removes the occurrences of the passed char in the beggining of the
     * string.
     */
    public static String rightTrim(final String input, final char charToTrim) {
        int len = input.length();
        final int st = 0;
        final int off = 0;
        final char[] val = input.toCharArray();

        while (st < len && val[off + len - 1] == charToTrim) {
            len--;
        }
        return input.substring(0, len);
    }

    /**
     * Removes the occurrences of the passed char in the start and end of the
     * string.
     */
    public static String leftAndRightTrim(final String input,
            final char charToTrim) {
        return rightTrim(leftTrim(input, charToTrim), charToTrim);
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
     * Changes all backward slashes (\) for forward slashes (/)
     * 
     * @return the replaced string
     */
    public static String replaceAllSlashes(final String string) {
        final int len = string.length();
        char c = 0;

        for (int i = 0; i < len; i++) {
            c = string.charAt(i);

            if (c == '\\') { // only do some processing if there is a
                // backward slash
                final char[] ds = string.toCharArray();
                ds[i] = '/';
                for (int j = i; j < len; j++) {
                    if (ds[j] == '\\') {
                        ds[j] = '/';
                    }
                }
                return new String(ds);
            }

        }
        return string;
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
        final ArrayList<String> ret = new ArrayList<String>();
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

    public static boolean isSingleWord(final String string) {
        for (final char c : string.toCharArray()) {
            if (!Character.isJavaIdentifierStart(c)) {
                return false;
            }
        }
        return true;
    }

    public static String replaceAll(final String string, final String replace,
            final String with) {
        final StringBuilder ret = new StringBuilder();
        final int len = string.length();
        final int replaceLen = replace.length();

        for (int i = 0; i < len; i++) {
            if (i + replaceLen > len) {
                ret.append(string.charAt(i));
                continue;
            }
            final String s = string.substring(i, i + replaceLen);
            if (s.equals(replace)) {
                ret.append(with);
                i = i + replaceLen - 1;
            } else {
                ret.append(s.charAt(0));
            }
        }

        return ret.toString();
    }

    /**
     * Splits the passed string based on the toSplit string.
     */
    public static List<String> split(final String delimiter, final String string) {
        if (delimiter.length() == 1) {
            return split(string, delimiter.charAt(0));
        }
        final ArrayList<String> ret = new ArrayList<String>();
        if (delimiter.length() == 0) {
            ret.add(string);
            return ret;
        }

        final int len = string.length();

        int last = 0;

        char c = 0;

        for (int i = 0; i < len; i++) {
            c = string.charAt(i);
            if (c == delimiter.charAt(0) && matches(string, delimiter, i)) {
                if (last != i) {
                    ret.add(string.substring(last, i));
                }
                last = i + delimiter.length();
                i += delimiter.length() - 1;
            }
        }

        if (last < len) {
            ret.add(string.substring(last, len));
        }

        return ret;
    }

    private static boolean matches(final String string, final String toSplit,
            final int i) {
        if (string.length() - i >= toSplit.length()) {
            for (int j = 0; j < toSplit.length(); j++) {
                if (string.charAt(i + j) != toSplit.charAt(j)) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    /**
     * Splits some string given some char
     */
    public static List<String> split(final char delimiter, final String string) {
        final ArrayList<String> ret = new ArrayList<String>();
        final int len = string.length();

        int last = 0;

        char c = 0;

        for (int i = 0; i < len; i++) {
            c = string.charAt(i);
            if (c == delimiter) {
                if (last != i) {
                    ret.add(string.substring(last, i));
                }
                while (c == delimiter && i < len - 1) {
                    i++;
                    c = string.charAt(i);
                }
                last = i;
            }
        }
        if (c != delimiter) {
            if (last == 0 && len > 0) {
                ret.add(string); // it is equal to the original (no dots)

            } else if (last < len) {
                ret.add(string.substring(last, len));

            }
        }
        return ret;
    }

    /**
     * Splits some string given many chars
     */
    public static List<String> split(final String string, final char... toSplit) {
        final ArrayList<String> ret = new ArrayList<String>();
        final int len = string.length();

        int last = 0;

        char c = 0;

        for (int i = 0; i < len; i++) {
            c = string.charAt(i);

            if (contains(c, toSplit)) {
                if (last != i) {
                    ret.add(string.substring(last, i));
                }
                while (contains(c, toSplit) && i < len - 1) {
                    i++;
                    c = string.charAt(i);
                }
                last = i;
            }
        }
        if (!contains(c, toSplit)) {
            if (last == 0 && len > 0) {
                ret.add(string); // it is equal to the original (no dots)

            } else if (last < len) {
                ret.add(string.substring(last, len));

            }
        }
        return ret;
    }

    public static List<String> splitAndRemoveEmptyTrimmed(final char c,
            final String string) {
        final List<String> split = split(c, string);
        for (int i = split.size() - 1; i >= 0; i--) {
            if (split.get(i).trim().length() == 0) {
                split.remove(i);
            }
        }
        return split;
    }

    private static boolean contains(final char c, final char[] toSplit) {
        for (final char ch : toSplit) {
            if (c == ch) {
                return true;
            }
        }
        return false;
    }

    /**
     * Splits some string given some char in 2 parts. If the separator is not
     * found, everything is put in the 1st part.
     */
    public static Tuple<String, String> splitOnFirst(final String fullRep,
            final char toSplit) {
        final int i = fullRep.indexOf(toSplit);
        if (i != -1) {
            return new Tuple<String, String>(fullRep.substring(0, i),
                    fullRep.substring(i + 1));
        } else {
            return new Tuple<String, String>(fullRep, "");
        }

    }

    /**
     * Splits the string as would string.split("\\."), but without yielding
     * empty strings
     */
    public static List<String> dotSplit(final String string) {
        return splitAndRemoveEmptyTrimmed('.', string);
    }

    /**
     * Same as Python join: Go through all the paths in the string and join them
     * with the passed delimiter.
     */
    public static String join(final String delimiter, final String[] splitted) {
        final StringBuilder buf = new StringBuilder(splitted.length * 100);
        for (final String string : splitted) {
            if (buf.length() > 0) {
                buf.append(delimiter);
            }
            buf.append(string);
        }
        return buf.toString();
    }

    /**
     * Same as Python join: Go through all the paths in the string and join them
     * with the passed delimiter.
     */
    public static String join(final String delimiter,
            final List<String> splitted) {
        final StringBuilder buf = new StringBuilder(splitted.size() * 100);
        for (final String string : splitted) {
            if (buf.length() > 0) {
                buf.append(delimiter);
            }
            buf.append(string);
        }
        return buf.toString();
    }

    /**
     * Adds a char to an array of chars and returns the new array.
     * 
     * @param c
     *            The chars to where the new char should be appended
     * @param toAdd
     *            the char to be added
     * @return a new array with the passed char appended.
     */
    public static char[] appendChar(final char[] c, final char toAdd) {
        final char[] c1 = new char[c.length + 1];

        System.arraycopy(c, 0, c1, 0, c.length);
        c1[c.length] = toAdd;
        return c1;

    }

    public static String replaceNewLines(String message, final String string) {
        message = message.replaceAll("\r\n", string);
        message = message.replaceAll("\r", string);
        message = message.replaceAll("\n", string);

        return message;
    }

    public static String removeNewLineChars(final String message) {
        return message.replaceAll("\r", "").replaceAll("\n", "");
    }

    public static boolean equalFilePaths(String path_1, String path_2) {
        if (!EFS.getLocalFileSystem().isCaseSensitive()) {
            path_1 = path_1.toLowerCase();
            path_2 = path_2.toLowerCase();
        }
        final Path path1 = new Path(path_1);
        final Path path2 = new Path(path_2);
        return path1.equals(path2);
    }
}
