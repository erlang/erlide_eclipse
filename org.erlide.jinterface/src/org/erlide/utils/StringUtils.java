/*
 * Code borrowed from PyDev
 */
/*
 * @author Fabio Zadrozny
 * Created: June 2005
 * License: Common Public License v1.0
 */

package org.erlide.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Scanner;

import com.google.common.collect.Lists;

public class StringUtils {

    private StringUtils() {
    }

    public static final Object EMPTY = "";

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

    public static String withoutInterrogationMark(final String definedName) {
        if (definedName.startsWith("?")) {
            return definedName.substring(1);
        }
        return definedName;
    }

    public static String unquote(final String s) {
        final int length = s.length();
        if (length > 2 && s.charAt(0) == '\'' && s.charAt(length - 1) == '\'') {
            return s.substring(1, length - 1);
        } else {
            return s;
        }
    }

    public static String getLongestPrefix(final List<String> list) {
        final StringBuilder b = new StringBuilder();
        int i = 0;
        for (;;) {
            final Character c = checkCharAt(i, list);
            if (c == null) {
                break;
            } else {
                b.append(c);
            }
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
                } else {
                    if (c != s.charAt(i)) {
                        return null;
                    }
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
        final int prefixLength = getLongestPrefix(list).length();
        if (prefixLength == 0) {
            return list;
        }
        final List<String> result = Lists.newArrayList();
        for (final String s : list) {
            result.add(s.substring(prefixLength));
        }
        return result;
    }

    public static String convertStreamToString(final java.io.InputStream is) {
        try {
            final Scanner scanner = new Scanner(is).useDelimiter("\\A");
            if (scanner.hasNext()) {
                return scanner.next();
            }
            return "";
        } catch (final NoSuchElementException e) {
            return "";
        }
    }

}
