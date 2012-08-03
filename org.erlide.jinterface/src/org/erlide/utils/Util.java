/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.utils;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.DataInput;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.UTFDataFormatException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharsetEncoder;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.StringTokenizer;

import org.eclipse.core.runtime.Assert;
import org.erlide.jinterface.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.base.Charsets;

/**
 * Provides convenient utility methods to other types in this package.
 */
public final class Util {

    private static final String ARGUMENTS_DELIMITER = "#"; //$NON-NLS-1$

    /* Bundle containing messages */
    static ResourceBundle bundle;

    private static final String BUNDLE_NAME = "org.erlide.utils.util"; //$NON-NLS-1$

    private static final char[] DOUBLE_QUOTES = "''".toCharArray(); //$NON-NLS-1$

    private static final String EMPTY_ARGUMENT = "   "; //$NON-NLS-1$

    // public static final String[] fgEmptyStringArray = new String[0];

    private static final char[] SINGLE_QUOTE = "'".toCharArray(); //$NON-NLS-1$

    static {
        relocalize();
    }

    private Util() {
        // cannot be instantiated
    }

    /**
     * Lookup the message with the given PLUGIN_ID in this catalog
     */
    public static String bind(final String id) {
        return bind(id, (String[]) null);
    }

    /**
     * Lookup the message with the given PLUGIN_ID in this catalog and bind its
     * substitution locations with the given string.
     */
    public static String bind(final String id, final String binding) {
        return bind(id, new String[] { binding });
    }

    /**
     * Lookup the message with the given PLUGIN_ID in this catalog and bind its
     * substitution locations with the given strings.
     */
    public static String bind(final String id, final String binding1,
            final String binding2) {
        return bind(id, new String[] { binding1, binding2 });
    }

    /**
     * Lookup the message with the given PLUGIN_ID in this catalog and bind its
     * substitution locations with the given string values.
     */
    public static String bind(final String id, final String[] bindings) {
        if (id == null) {
            return "No message available"; //$NON-NLS-1$
        }
        String message = null;
        try {
            message = bundle.getString(id);
        } catch (final MissingResourceException e) {
            // If we got an exception looking for the message, fail gracefully
            // by just returning
            // the id we were looking for. In most cases this is
            // semi-informative so is not too bad.
            return "Missing message: " + id + " in: " + BUNDLE_NAME; //$NON-NLS-2$ //$NON-NLS-1$
        }
        // for compatibility with MessageFormat which eliminates double quotes
        // in original message
        final char[] messageWithNoDoubleQuotes = CharOperation.replace(
                message.toCharArray(), DOUBLE_QUOTES, SINGLE_QUOTE);

        if (bindings == null) {
            return new String(messageWithNoDoubleQuotes);
        }

        final int length = messageWithNoDoubleQuotes.length;
        int start = 0;
        int end = length;
        final StringBuilder output = new StringBuilder();
        while (true) {
            end = CharOperation.indexOf('{', messageWithNoDoubleQuotes, start);
            if (end > -1) {
                output.append(messageWithNoDoubleQuotes, start, end - start);
                if ((start = CharOperation.indexOf('}',
                        messageWithNoDoubleQuotes, end + 1)) > -1) {
                    int index = -1;
                    final String argId = new String(messageWithNoDoubleQuotes,
                            end + 1, start - end - 1);
                    try {
                        index = Integer.parseInt(argId);
                        output.append(bindings[index]);
                    } catch (final NumberFormatException nfe) { // could be
                        // nested
                        // message PLUGIN_ID
                        // {compiler.name}
                        boolean done = false;
                        if (!argId.equals(id)) {
                            String argMessage = null;
                            try {
                                argMessage = bundle.getString(argId);
                                output.append(argMessage);
                                done = true;
                            } catch (final MissingResourceException e) {
                                // unable to bind argument, ignore (will leave
                                // argument in)
                            }
                        }
                        if (!done) {
                            output.append(messageWithNoDoubleQuotes, end + 1,
                                    start - end);
                        }
                    } catch (final ArrayIndexOutOfBoundsException e) {
                        output.append("{missing " + Integer.toString(index) + "}"); //$NON-NLS-2$ //$NON-NLS-1$
                    }
                    start++;
                } else {
                    output.append(messageWithNoDoubleQuotes, end, length);
                    break;
                }
            } else {
                output.append(messageWithNoDoubleQuotes, start, length - start);
                break;
            }
        }
        return output.toString();
    }

    /**
     * Compares two byte arrays. Returns <0 if a byte in a is less than the
     * corresponding byte in b, or if a is shorter, or if a is null. Returns >0
     * if a byte in a is greater than the corresponding byte in b, or if a is
     * longer, or if b is null. Returns 0 if they are equal or both null.
     */
    public static int compare(final byte[] a, final byte[] b) {
        if (a == b) {
            return 0;
        }
        if (a == null) {
            return -1;
        }
        if (b == null) {
            return 1;
        }
        final int len = Math.min(a.length, b.length);
        for (int i = 0; i < len; ++i) {
            final int diff = a[i] - b[i];
            if (diff != 0) {
                return diff;
            }
        }
        if (a.length > len) {
            return 1;
        }
        if (b.length > len) {
            return -1;
        }
        return 0;
    }

    /**
     * Compares two strings lexicographically. The comparison is based on the
     * Unicode value of each character in the strings.
     * 
     * @return the value <code>0</code> if the str1 is equal to str2; a value
     *         less than <code>0</code> if str1 is lexicographically less than
     *         str2; and a value greater than <code>0</code> if str1 is
     *         lexicographically greater than str2.
     */
    public static int compare(final char[] str1, final char[] str2) {
        final int len1 = str1.length;
        final int len2 = str2.length;
        int n = Math.min(len1, len2);
        int i = 0;
        while (n-- != 0) {
            final char c1 = str1[i];
            final char c2 = str2[i++];
            if (c1 != c2) {
                return c1 - c2;
            }
        }
        return len1 - len2;
    }

    /**
     * Concatenate two strings with a char in between.
     * 
     * @see #concat(String, String)
     */
    public static String concat(String s1, final char c, String s2) {
        if (s1 == null) {
            s1 = "null"; //$NON-NLS-1$
        }
        if (s2 == null) {
            s2 = "null"; //$NON-NLS-1$
        }
        final int l1 = s1.length();
        final int l2 = s2.length();
        final char[] buf = new char[l1 + 1 + l2];
        s1.getChars(0, l1, buf, 0);
        buf[l1] = c;
        s2.getChars(0, l2, buf, l1 + 1);
        return new String(buf);
    }

    /**
     * Concatenate two strings. Much faster than using +, which: - creates a
     * StringBuilder, - which is synchronized, - of default size, so the
     * resulting char array is often larger than needed. This implementation
     * creates an extra char array, since the String constructor copies its
     * argument, but there's no way around this.
     */
    public static String concat(String s1, String s2) {
        if (s1 == null) {
            s1 = "null"; //$NON-NLS-1$
        }
        if (s2 == null) {
            s2 = "null"; //$NON-NLS-1$
        }
        final int l1 = s1.length();
        final int l2 = s2.length();
        final char[] buf = new char[l1 + l2];
        s1.getChars(0, l1, buf, 0);
        s2.getChars(0, l2, buf, l1);
        return new String(buf);
    }

    /**
     * Concatenate three strings.
     * 
     * @see #concat(String, String)
     */
    public static String concat(String s1, String s2, String s3) {
        if (s1 == null) {
            s1 = "null"; //$NON-NLS-1$
        }
        if (s2 == null) {
            s2 = "null"; //$NON-NLS-1$
        }
        if (s3 == null) {
            s3 = "null"; //$NON-NLS-1$
        }
        final int l1 = s1.length();
        final int l2 = s2.length();
        final int l3 = s3.length();
        final char[] buf = new char[l1 + l2 + l3];
        s1.getChars(0, l1, buf, 0);
        s2.getChars(0, l2, buf, l1);
        s3.getChars(0, l3, buf, l1 + l2);
        return new String(buf);
    }

    /**
     * Returns true iff str.toLowerCase().endsWith(end.toLowerCase())
     * implementation is not creating extra strings.
     */
    public static final boolean endsWithIgnoreCase(final String str,
            final String end) {

        if (str == null && end == null) {
            return true;
        }
        if (str == null) {
            return false;
        }
        if (end == null) {
            return false;
        }
        if (str.equals(end)) {
            return true;
        }

        final int strLength = str.length();
        final int endLength = end.length();

        // return false if the string is smaller than the end.
        if (endLength > strLength) {
            return false;
        }

        // return false if any character of the end are
        // not the same in lower case.
        for (int i = 1; i <= endLength; i++) {
            if (Character.toLowerCase(end.charAt(endLength - i)) != Character
                    .toLowerCase(str.charAt(strLength - i))) {
                return false;
            }
        }

        return true;
    }

    /**
     * Compares two arrays using equals() on the elements. Either or both arrays
     * may be null. Returns true if both are null. Returns false if only one is
     * null. If both are arrays, returns true iff they have the same length and
     * all elements are equal.
     */
    public static boolean equalArraysOrNull(final int[] a, final int[] b) {
        if (a == b) {
            return true;
        }
        if (a == null || b == null) {
            return false;
        }
        final int len = a.length;
        if (len != b.length) {
            return false;
        }
        for (int i = 0; i < len; ++i) {
            if (a[i] != b[i]) {
                return false;
            }
        }
        return true;
    }

    /**
     * Compares two arrays using equals() on the elements. Either or both arrays
     * may be null. Returns true if both are null. Returns false if only one is
     * null. If both are arrays, returns true iff they have the same length and
     * all elements compare true with equals.
     */
    public static boolean equalArraysOrNull(final Object[] a, final Object[] b) {
        if (a == b) {
            return true;
        }
        if (a == null || b == null) {
            return false;
        }

        final int len = a.length;
        if (len != b.length) {
            return false;
        }
        for (int i = 0; i < len; ++i) {
            if (a[i] == null) {
                if (b[i] != null) {
                    return false;
                }
            } else {
                if (!a[i].equals(b[i])) {
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * Compares two objects using equals(). Either or both array may be null.
     * Returns true if both are null. Returns false if only one is null.
     * Otherwise, return the result of comparing with equals().
     */
    public static boolean equalOrNull(final Object a, final Object b) {
        if (a == b) {
            return true;
        }
        if (a == null || b == null) {
            return false;
        }
        return a.equals(b);
    }

    /**
     * Given a qualified name, extract the last component. If the input is not
     * qualified, the same string is answered.
     */
    public static String extractLastName(final String qualifiedName) {
        final int i = qualifiedName.lastIndexOf('.');
        if (i == -1) {
            return qualifiedName;
        }
        return qualifiedName.substring(i + 1);
    }

    /**
     * Finds the first line separator used by the given text.
     * 
     * @return</code> "\n"</code> or</code> "\r"</code> or</code> "\r\n"
     *                </code>, or <code>null</code> if none found
     */
    public static String findLineSeparator(final char[] text) {
        // find the first line separator
        final int length = text.length;
        if (length > 0) {
            char nextChar = text[0];
            for (int i = 0; i < length; i++) {
                final char currentChar = nextChar;
                nextChar = i < length - 1 ? text[i + 1] : ' ';
                if (currentChar == '\n') {
                    return "\n"; //$NON-NLS-1$
                }
                if (currentChar == '\r') {
                    return nextChar == '\n' ? "\r\n" : "\r"; //$NON-NLS-1$ //$NON-NLS-2$
                }

            }
        }
        // not found
        return null;
    }

    // /**
    // * Returns the line separator used by the given buffer. Uses the given
    // text
    // * if none found.
    // *
    // * @return</code> "\n"</code> or</code> "\r"</code> or</code> "\r\n"
    // * </code>
    // */
    // private static String getLineSeparator(char[] text, char[] buffer) {
    // // search in this buffer's contents first
    // String lineSeparator = findLineSeparator(buffer);
    // if (lineSeparator == null) {
    // // search in the given text
    // lineSeparator = findLineSeparator(text);
    // if (lineSeparator == null) {
    // // default to system line separator
    // return "\n";
    // }
    // }
    // return lineSeparator;
    // }

    /**
     * Returns the number of parameter types in a method signature.
     */
    public static int getParameterCount(final char[] sig) {
        int i = CharOperation.indexOf('(', sig) + 1;
        Assert.isTrue(i != 0);
        int count = 0;
        final int len = sig.length;
        for (;;) {
            if (i == len) {
                break;
            }
            final char c = sig[i];
            if (c == ')') {
                break;
            }
            if (c == '[') {
                ++i;
            } else if (c == 'L') {
                ++count;
                i = CharOperation.indexOf(';', sig, i + 1) + 1;
                Assert.isTrue(i != 0);
            } else {
                ++count;
                ++i;
            }
        }
        return count;
    }

    /**
     * Put all the arguments in one String.
     */
    public static String getProblemArgumentsForMarker(final String[] arguments) {
        final StringBuilder args = new StringBuilder(10);

        args.append(arguments.length);
        args.append(':');

        for (int j = 0; j < arguments.length; j++) {
            if (j != 0) {
                args.append(ARGUMENTS_DELIMITER);
            }

            if (arguments[j].length() == 0) {
                args.append(EMPTY_ARGUMENT);
            } else {
                args.append(arguments[j]);
            }
        }

        return args.toString();
    }

    /**
     * Separate all the arguments of a String made by
     * getProblemArgumentsForMarker
     */
    public static String[] getProblemArgumentsFromMarker(String argumentsString) {
        if (argumentsString == null) {
            return null;
        }
        final int index = argumentsString.indexOf(':');
        if (index == -1) {
            return null;
        }

        final int length = argumentsString.length();
        int numberOfArg;
        try {
            numberOfArg = Integer.parseInt(argumentsString.substring(0, index));
        } catch (final NumberFormatException e) {
            return null;
        }
        argumentsString = argumentsString.substring(index + 1, length);

        String[] args = new String[length];
        int count = 0;

        final StringTokenizer tokenizer = new StringTokenizer(argumentsString,
                ARGUMENTS_DELIMITER);
        while (tokenizer.hasMoreTokens()) {
            String argument = tokenizer.nextToken();
            if (EMPTY_ARGUMENT.equals(argument)) {
                argument = ""; //$NON-NLS-1$
            }
            args[count++] = argument;
        }

        if (count != numberOfArg) {
            return null;
        }

        System.arraycopy(args, 0, args = new String[count], 0, count);
        return args;
    }

    /**
     * Validate the given compilation unit name. A compilation unit name must
     * obey the following rules:
     * <ul>
     * <li>it must not be null
     * <li>it must include the <code>".erl"</code> suffix
     * <li>its prefix must be a valid identifier
     * </ul>
     * </p>
     * 
     * @param name
     *            the name of a compilation unit
     * @return a boolean
     */
    public static boolean isValidModuleFileName(final String name) {
        if (!name.endsWith(".erl")) {
            return false;
        }
        final int pos = name.lastIndexOf('.');
        return isValidModuleName(name.substring(0, pos));
    }

    public static boolean isValidModuleName(final String name) {
        return name.matches("[a-z][a-zA-Z0-9_]*");
    }

    // /**
    // * Normalizes the cariage returns in the given text. They are all changed
    // to
    // * use the given buffer's line separator.
    // */
    // public static char[] normalizeCRs(char[] text, char[] buffer) {
    // final CharArrayBuffer result = new CharArrayBuffer();
    // int lineStart = 0;
    // final int length = text.length;
    // if (length == 0) {
    // return text;
    // }
    // final String lineSeparator = getLineSeparator(text, buffer);
    // char nextChar = text[0];
    // for (int i = 0; i < length; i++) {
    // final char currentChar = nextChar;
    // nextChar = (i < length - 1) ? text[i + 1] : ' ';
    // switch (currentChar) {
    // case '\n':
    // int lineLength = i - lineStart;
    // char[] line = new char[lineLength];
    // System.arraycopy(text, lineStart, line, 0, lineLength);
    // result.append(line);
    // result.append(lineSeparator);
    // lineStart = i + 1;
    // break;
    // case '\r':
    // lineLength = i - lineStart;
    // if (lineLength >= 0) {
    // line = new char[lineLength];
    // System.arraycopy(text, lineStart, line, 0, lineLength);
    // result.append(line);
    // result.append(lineSeparator);
    // if (nextChar == '\n') {
    // nextChar = ' ';
    // lineStart = i + 2;
    // } else {
    // // when line separator are mixed in the same file
    // // \r might not be followed by a \n. If not, we
    // // should increment
    // // lineStart by one and not by two.
    // lineStart = i + 1;
    // }
    // } else {
    // // when line separator are mixed in the same file
    // // we need to prevent NegativeArraySizeException
    // lineStart = i + 1;
    // }
    // break;
    // }
    // }
    // char[] lastLine;
    // if (lineStart > 0) {
    // final int lastLineLength = length - lineStart;
    // if (lastLineLength > 0) {
    // lastLine = new char[lastLineLength];
    // System.arraycopy(text, lineStart, lastLine, 0, lastLineLength);
    // result.append(lastLine);
    // }
    // return result.getContents();
    // }
    // return text;
    // }
    //
    // /**
    // * Normalizes the cariage returns in the given text. They are all changed
    // to
    // * use given buffer's line sepatator.
    // */
    // public static String normalizeCRs(String text, String buffer) {
    // return new String(
    // normalizeCRs(text.toCharArray(), buffer.toCharArray()));
    // }

    /**
     * Returns the length of the common prefix between s1 and s2.
     */
    public static int prefixLength(final char[] s1, final char[] s2) {
        int len = 0;
        final int max = Math.min(s1.length, s2.length);
        for (int i = 0; i < max && s1[i] == s2[i]; ++i) {
            ++len;
        }
        return len;
    }

    /**
     * Returns the length of the common prefix between s1 and s2.
     */
    public static int prefixLength(final String s1, final String s2) {
        int len = 0;
        final int max = Math.min(s1.length(), s2.length());
        for (int i = 0; i < max && s1.charAt(i) == s2.charAt(i); ++i) {
            ++len;
        }
        return len;
    }

    /**
     * Reads in a string from the specified data input stream. The string has
     * been encoded using a modified UTF-8 format.
     * <p>
     * The first two bytes are read as if by <code>readUnsignedShort</code>.
     * This value gives the number of following bytes that are in the encoded
     * string, not the length of the resulting string. The following bytes are
     * then interpreted as bytes encoding characters in the UTF-8 format and are
     * converted into characters.
     * <p>
     * This method blocks until all the bytes are read, the end of the stream is
     * detected, or an exception is thrown.
     * 
     * @param in
     *            a data input stream.
     * @return a Unicode string.
     * @exception EOFException
     *                if the input stream reaches the end before all the bytes.
     * @exception IOException
     *                if an I/O error occurs.
     * @exception UTFDataFormatException
     *                if the bytes do not represent a valid UTF-8 encoding of a
     *                Unicode string.
     * @see java.io.DataInputStream#readUnsignedShort()
     */
    static final char[] readUTF(final DataInput in) throws IOException {
        final int utflen = in.readUnsignedShort();
        char[] str = new char[utflen];
        int count = 0;
        int strlen = 0;
        while (count < utflen) {
            final int c = in.readUnsignedByte();
            int char2, char3;
            switch (c >> 4) {
            case 0:
            case 1:
            case 2:
            case 3:
            case 4:
            case 5:
            case 6:
            case 7:
                // 0xxxxxxx
                count++;
                str[strlen++] = (char) c;
                break;
            case 12:
            case 13:
                // 110x xxxx 10xx xxxx
                count += 2;
                if (count > utflen) {
                    throw new UTFDataFormatException();
                }
                char2 = in.readUnsignedByte();
                if ((char2 & 0xC0) != 0x80) {
                    throw new UTFDataFormatException();
                }
                str[strlen++] = (char) ((c & 0x1F) << 6 | char2 & 0x3F);
                break;
            case 14:
                // 1110 xxxx 10xx xxxx 10xx xxxx
                count += 3;
                if (count > utflen) {
                    throw new UTFDataFormatException();
                }
                char2 = in.readUnsignedByte();
                char3 = in.readUnsignedByte();
                if ((char2 & 0xC0) != 0x80 || (char3 & 0xC0) != 0x80) {
                    throw new UTFDataFormatException();
                }
                str[strlen++] = (char) ((c & 0x0F) << 12 | (char2 & 0x3F) << 6 | (char3 & 0x3F) << 0);
                break;
            default:
                // 10xx xxxx, 1111 xxxx
                throw new UTFDataFormatException();
            }
        }
        if (strlen < utflen) {
            System.arraycopy(str, 0, str = new char[strlen], 0, strlen);
        }
        return str;
    }

    /**
     * Creates a NLS catalog for the given locale.
     */
    public static void relocalize() {
        try {
            bundle = ResourceBundle.getBundle(BUNDLE_NAME, Locale.getDefault());
        } catch (final MissingResourceException e) {
            System.out
                    .println("Missing resource : " + BUNDLE_NAME.replace('.', '/') + ".properties for locale " + Locale.getDefault()); //$NON-NLS-1$//$NON-NLS-2$
            throw e;
        }
    }

    /**
     * Converts a String[] to char[][].
     */
    public static char[][] toCharArrays(final String[] a) {
        final int len = a.length;
        final char[][] result = new char[len][];
        for (int i = 0; i < len; ++i) {
            result[i] = toChars(a[i]);
        }
        return result;
    }

    /**
     * Converts a String to char[].
     */
    public static char[] toChars(final String s) {
        final int len = s.length();
        final char[] chars = new char[len];
        s.getChars(0, len, chars, 0);
        return chars;
    }

    /**
     * Converts a String to char[][], where segments are separate by '.'.
     */
    public static char[][] toCompoundChars(final String s) {
        final int len = s.length();
        if (len == 0) {
            return CharOperation.NO_CHAR_CHAR;
        }
        int segCount = 1;
        for (int off = s.indexOf('.'); off != -1; off = s.indexOf('.', off + 1)) {
            ++segCount;
        }
        final char[][] segs = new char[segCount][];
        int start = 0;
        for (int i = 0; i < segCount; ++i) {
            final int dot = s.indexOf('.', start);
            final int end = dot == -1 ? s.length() : dot;
            segs[i] = new char[end - start];
            s.getChars(start, end, segs[i], 0);
            start = end + 1;
        }
        return segs;
    }

    /**
     * Converts a char[] to String.
     */
    public static String toString(final char[] c) {
        return new String(c);
    }

    /**
     * Converts a char[][] to String, where segments are separated by '.'.
     */
    public static String toString(final char[][] c) {
        final StringBuilder sb = new StringBuilder();
        for (int i = 0, max = c.length; i < max; ++i) {
            if (i != 0) {
                sb.append('.');
            }
            sb.append(c[i]);
        }
        return sb.toString();
    }

    /**
     * Converts a char[][] and a char[] to String, where segments are separated
     * by '.'.
     */
    public static String toString(final char[][] c, final char[] d) {
        if (c == null) {
            return new String(d);
        }
        final StringBuilder sb = new StringBuilder();
        for (final char[] element : c) {
            sb.append(element);
            sb.append('.');
        }
        sb.append(d);
        return sb.toString();
    }

    /**
     * Writes a string to the given output stream using UTF-8 encoding in a
     * machine-independent manner.
     * <p>
     * First, two bytes are written to the output stream as if by the
     * <code>writeShort</code> method giving the number of bytes to follow. This
     * value is the number of bytes actually written out, not the length of the
     * string. Following the length, each character of the string is output, in
     * sequence, using the UTF-8 encoding for the character.
     * 
     * @param str
     *            a string to be written.
     * @return the number of bytes written to the stream.
     * @exception IOException
     *                if an I/O error occurs.
     * 
     */
    public static int writeUTF(final OutputStream out, final char[] str)
            throws IOException {
        final int strlen = str.length;
        int utflen = 0;
        for (int i = 0; i < strlen; i++) {
            final int c = str[i];
            if (c >= 0x0001 && c <= 0x007F) {
                utflen++;
            } else if (c > 0x07FF) {
                utflen += 3;
            } else {
                utflen += 2;
            }
        }
        if (utflen > 65535) {
            throw new UTFDataFormatException();
        }
        out.write(utflen >>> 8 & 0xFF);
        out.write(utflen >>> 0 & 0xFF);
        if (strlen == utflen) {
            for (int i = 0; i < strlen; i++) {
                out.write(str[i]);
            }
        } else {
            for (int i = 0; i < strlen; i++) {
                final int c = str[i];
                if (c >= 0x0001 && c <= 0x007F) {
                    out.write(c);
                } else if (c > 0x07FF) {
                    out.write(0xE0 | c >> 12 & 0x0F);
                    out.write(0x80 | c >> 6 & 0x3F);
                    out.write(0x80 | c >> 0 & 0x3F);
                } else {
                    out.write(0xC0 | c >> 6 & 0x1F);
                    out.write(0x80 | c >> 0 & 0x3F);
                }
            }
        }
        return utflen + 2; // the number of bytes written to the stream
    }

    public interface Displayable {

        String displayString(Object o);
    }

    public static final String LINE_SEPARATOR = System
            .getProperty("line.separator"); //$NON-NLS-1$

    static final char[] LINE_SEPARATOR_CHARS = LINE_SEPARATOR.toCharArray();

    private static final int DEFAULT_READING_SIZE = 8192;

    /**
     * Returns the given bytes as a char array using a given encoding (null
     * means platform default).
     */
    public static char[] bytesToChar(final byte[] bytes, final Charset encoding)
            throws IOException {
        return getInputStreamAsCharArray(new ByteArrayInputStream(bytes),
                bytes.length, encoding);
    }

    /**
     * Returns the contents of the given file as a byte array.
     * 
     * @throws IOException
     *             if a problem occured reading the file.
     */
    public static byte[] getFileByteContent(final File file) throws IOException {
        InputStream stream = null;
        try {
            stream = new BufferedInputStream(new FileInputStream(file));
            return getInputStreamAsByteArray(stream, (int) file.length());
        } finally {
            if (stream != null) {
                try {
                    stream.close();
                } catch (final IOException e) {
                    // ignore
                }
            }
        }
    }

    /**
     * Returns the contents of the given file as a char array. When encoding is
     * null, then the platform default one is used
     * 
     * @throws IOException
     *             if a problem occured reading the file.
     */
    public static char[] getFileCharContent(final File file,
            final Charset encoding) throws IOException {
        InputStream stream = null;
        try {
            stream = new BufferedInputStream(new FileInputStream(file));
            return getInputStreamAsCharArray(stream, (int) file.length(),
                    encoding);
        } finally {
            if (stream != null) {
                try {
                    stream.close();
                } catch (final IOException e) {
                    // ignore
                }
            }
        }
    }

    public static char[] getFileCharContent(final String path,
            final Charset encoding) throws IOException {
        InputStream stream = null;
        try {
            stream = new BufferedInputStream(new FileInputStream(path));
            return getInputStreamAsCharArray(stream, -1, encoding);
        } finally {
            if (stream != null) {
                try {
                    stream.close();
                } catch (final IOException e) {
                }
            }
        }
    }

    /*
     * NIO support to get input stream as byte array. Not used as with JDK 1.4.2
     * this support is slower than standard IO one... Keep it as comment for
     * future in case of next JDK versions improve performance in this area...
     * 
     * public static byte[] getInputStreamAsByteArray(FileInputStream stream,
     * int length) throws IOException {
     * 
     * FileChannel channel = stream.getChannel(); int size =
     * (int)channel.size(); if (length >= 0 && length < size) size = length;
     * byte[] contents = new byte[size]; ByteBuffer buffer =
     * ByteBuffer.wrap(contents); channel.read(buffer); return contents; }
     */
    /**
     * Returns the given input stream's contents as a byte array. If a length is
     * specified (ie. if length != -1), only length bytes are returned.
     * Otherwise all bytes in the stream are returned. Note this doesn't close
     * the stream.
     * 
     * @throws IOException
     *             if a problem occured reading the stream.
     */
    public static byte[] getInputStreamAsByteArray(final InputStream stream,
            final int length) throws IOException {
        byte[] contents;
        if (length == -1) {
            contents = new byte[0];
            int contentsLength = 0;
            int amountRead = -1;
            do {
                final int amountRequested = Math.max(stream.available(),
                        DEFAULT_READING_SIZE); // read
                // at
                // least
                // 8K

                // resize contents if needed
                if (contentsLength + amountRequested > contents.length) {
                    System.arraycopy(contents, 0,
                            contents = new byte[contentsLength
                                    + amountRequested], 0, contentsLength);
                }

                // read as many bytes as possible
                amountRead = stream.read(contents, contentsLength,
                        amountRequested);

                if (amountRead > 0) {
                    // remember length of contents
                    contentsLength += amountRead;
                }
            } while (amountRead != -1);

            // resize contents if necessary
            if (contentsLength < contents.length) {
                System.arraycopy(contents, 0,
                        contents = new byte[contentsLength], 0, contentsLength);
            }
        } else {
            contents = new byte[length];
            int len = 0;
            int readSize = 0;
            while (readSize != -1 && len != length) {
                // See PR 1FMS89U
                // We record first the read size. In this case len is the actual
                // read size.
                len += readSize;
                readSize = stream.read(contents, len, length - len);
            }
        }

        return contents;
    }

    /*
     * NIO support to get input stream as char array. Not used as with JDK 1.4.2
     * this support is slower than standard IO one... Keep it as comment for
     * future in case of next JDK versions improve performance in this area...
     * public static char[] getInputStreamAsCharArray(FileInputStream stream,
     * int length, String encoding) throws IOException {
     * 
     * FileChannel channel = stream.getChannel(); int size =
     * (int)channel.size(); if (length >= 0 && length < size) size = length;
     * Charset charset = encoding==null?systemCharset:Charset.forName(encoding);
     * if (charset != null) { MappedByteBuffer bbuffer =
     * channel.map(FileChannel.MapMode.READ_ONLY, 0, size); CharsetDecoder
     * decoder = charset.newDecoder(); CharBuffer buffer =
     * decoder.decode(bbuffer); char[] contents = new char[buffer.limit()];
     * buffer.get(contents); return contents; } throw new
     * UnsupportedCharsetException(SYSTEM_FILE_ENCODING); }
     */
    /**
     * Returns the given input stream's contents as a character array. If a
     * length is specified (ie. if length != -1), only length chars are
     * returned. Otherwise all chars in the stream are returned. Note this
     * doesn't close the stream.
     * 
     * @throws IOException
     *             if a problem occured reading the stream.
     */
    public static char[] getInputStreamAsCharArray(final InputStream stream,
            final int length, final Charset encoding) throws IOException {
        InputStreamReader reader = null;
        reader = encoding == null ? new InputStreamReader(stream)
                : new InputStreamReader(stream, encoding);
        char[] contents;
        try {
            if (length == -1) {
                contents = CharOperation.NO_CHAR;
                int contentsLength = 0;
                int amountRead = -1;
                do {
                    final int amountRequested = Math.max(stream.available(),
                            DEFAULT_READING_SIZE); // read
                    // at
                    // least
                    // 8K

                    // resize contents if needed
                    if (contentsLength + amountRequested > contents.length) {
                        final char[] old = contents;
                        contents = new char[contentsLength + amountRequested];
                        System.arraycopy(old, 0, contents, 0, contentsLength);
                    }

                    // read as many chars as possible
                    amountRead = reader.read(contents, contentsLength,
                            amountRequested);

                    if (amountRead > 0) {
                        // remember length of contents
                        contentsLength += amountRead;
                    }
                } while (amountRead != -1);

                // Do not keep first character for UTF-8 BOM encoding
                int start = 0;
                if (Charsets.UTF_8.equals(encoding)) {
                    if (contents[0] == 0xFEFF) { // if BOM char then skip
                        contentsLength--;
                        start = 1;
                    }
                }
                // resize contents if necessary
                if (contentsLength < contents.length) {
                    final char[] old = contents;
                    contents = new char[contentsLength];
                    System.arraycopy(old, start, contents, 0, contentsLength);
                }
            } else {
                contents = new char[length];
                int len = 0;
                int readSize = 0;
                while (readSize != -1 && len != length) {
                    // See PR 1FMS89U
                    // We record first the read size. In this case len is the
                    // actual
                    // read size.
                    len += readSize;
                    readSize = reader.read(contents, len, length - len);
                }
                // Do not keep first character for UTF-8 BOM encoding
                int start = 0;
                if (Charsets.UTF_8.equals(encoding)) {
                    if (contents[0] == 0xFEFF) { // if BOM char then skip
                        len--;
                        start = 1;
                    }
                }
                // See PR 1FMS89U
                // Now we need to resize in case the default encoding used more
                // than
                // one byte for each
                // character
                if (len != length) {
                    contents = new char[len];
                    System.arraycopy(contents, start, contents, 0, len);
                }
            }
        } finally {
            reader.close();
        }
        return contents;
    }

    /**
     * Converts an array of Objects into String.
     */
    public static String toString(final Object[] objects) {
        return toString(objects, new Displayable() {

            @Override
            public String displayString(final Object o) {
                if (o == null) {
                    return "null"; //$NON-NLS-1$
                }
                return o.toString();
            }
        });
    }

    /**
     * Converts an array of Objects into String.
     */
    public static String toString(final Object[] objects,
            final Displayable renderer) {
        if (objects == null) {
            return ""; //$NON-NLS-1$
        }
        final StringBuilder buffer = new StringBuilder(10);
        for (int i = 0; i < objects.length; i++) {
            if (i > 0) {
                buffer.append(", "); //$NON-NLS-1$
            }
            buffer.append(renderer.displayString(objects[i]));
        }
        return buffer.toString();
    }

    /**
     * Converts a boolean value into Boolean.
     * 
     * @param bool
     *            The boolean to convert
     * @return The corresponding Boolean object (TRUE or FALSE).
     */
    public static Boolean toBoolean(final boolean bool) {
        if (bool) {
            return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }

    /**
     * Get the string value of an Erlang string, empty if empty list
     * 
     * @param o
     *            Erlang string or list
     * @return string value
     */
    public static String stringValue(final OtpErlangObject o) {
        if (o instanceof OtpErlangString) {
            final OtpErlangString s = (OtpErlangString) o;
            return s.stringValue();
        } else if (o instanceof OtpErlangList) {
            final OtpErlangList l = (OtpErlangList) o;
            if (l.arity() == 0) {
                return "";
            }
            try {
                return l.stringValue();
            } catch (final OtpErlangException e) {
                ErlLogger.error(e);
                return null;
            }
        } else if (o instanceof OtpErlangBinary) {
            final OtpErlangBinary b = (OtpErlangBinary) o;
            String result;
            result = decode(b.binaryValue(), Charsets.UTF_8);
            if (result == null) {
                result = decode(b.binaryValue(), Charsets.ISO_8859_1);
            }
            if (result == null) {
                ErlLogger.error("bad binary value in stringValue"
                        + " (can't decode): " + o);
            }
            return result;
        }
        // ErlLogger.warn("bad value in stringValue: " + o);
        return null;
    }

    public static String decode(final byte[] binaryValue, final Charset charset) {
        final CharsetDecoder decoder = charset.newDecoder();
        try {
            final ByteBuffer bbuf = ByteBuffer.wrap(binaryValue);
            final CharBuffer cbuf = decoder.decode(bbuf);
            return cbuf.toString();
        } catch (final CharacterCodingException e) {
            return null;
        }
    }

    public static byte[] encode(final String string, final String encoding) {
        final Charset charset = Charset.forName(encoding);
        final CharsetEncoder encoder = charset.newEncoder();
        try {
            final CharBuffer cbuf = CharBuffer.wrap(string);
            final ByteBuffer bbuf = encoder.encode(cbuf);
            return bbuf.array();
        } catch (final CharacterCodingException e) {
            return null;
        }
    }

    public static OtpErlangList listValue(final OtpErlangObject o) {
        if (o instanceof OtpErlangList) {
            return (OtpErlangList) o;
        } else if (o instanceof OtpErlangString) {
            final OtpErlangString erlangString = (OtpErlangString) o;
            final int[] codePoints = OtpErlangString
                    .stringToCodePoints(erlangString.stringValue());
            final OtpErlangObject elements[] = new OtpErlangObject[codePoints.length];
            for (int i = 0; i < codePoints.length; i++) {
                elements[i] = new OtpErlangLong(codePoints[i]);
            }
            return new OtpErlangList(elements);
        }
        return null;
    }

    /**
     * Return true if it's the atom ok or a tuple {ok, ...}
     * 
     * @param o
     *            atom or tuple
     * @return true if ok
     */
    public static boolean isOk(final OtpErlangObject o) {
        return isTag(o, "ok");
    }

    /**
     * return true if it's the atom error or a tuple {error, ...}
     * 
     * @param o
     *            atom or tuple
     * @return true if error
     */
    public static boolean isError(final OtpErlangObject o) {
        return isTag(o, "error");
    }

    public static boolean isTag(final OtpErlangObject o, final String string) {
        OtpErlangAtom tag = null;
        if (o instanceof OtpErlangAtom) {
            tag = (OtpErlangAtom) o;
        } else if (o instanceof OtpErlangTuple) {
            final OtpErlangTuple t = (OtpErlangTuple) o;
            if (t.elementAt(0) instanceof OtpErlangAtom) {
                tag = (OtpErlangAtom) t.elementAt(0);
            }
        }
        return tag != null && string.equals(tag.atomValue());
    }

    public static String ioListToString(final OtpErlangObject o) {
        return ioListToString(o, Integer.MAX_VALUE - 1);
    }

    public static String ioListToString(final OtpErlangObject o,
            final int maxLength) {
        StringBuilder sb = new StringBuilder();
        sb = ioListToStringBuilder(o, sb, maxLength);
        return sb.toString();
    }

    public static String getInputStreamAsString(final InputStream is,
            final String encoding) {
        final StringBuilder out = new StringBuilder();
        final byte[] b = new byte[4096];
        try {
            for (int n; (n = is.read(b)) != -1;) {
                out.append(new String(b, 0, n, encoding));
            }
        } catch (final IOException e) {
        }
        return out.toString();
    }

    private static StringBuilder ioListToStringBuilder(final OtpErlangObject o,
            StringBuilder sb, final int maxLength) {
        if (sb.length() >= maxLength) {
            return sb;
        }
        if (o instanceof OtpErlangLong) {
            final OtpErlangLong l = (OtpErlangLong) o;
            try {
                sb.append(l.charValue());
            } catch (final OtpErlangRangeException e) {
            }
        } else if (o instanceof OtpErlangString) {
            final OtpErlangString s = (OtpErlangString) o;
            sb.append(s.stringValue());
        } else if (o instanceof OtpErlangList) {
            final OtpErlangList l = (OtpErlangList) o;
            for (final OtpErlangObject i : l) {
                if (sb.length() < maxLength) {
                    ioListToStringBuilder(i, sb, maxLength);
                }
            }
            if (sb.length() < maxLength) {
                ioListToStringBuilder(l.getLastTail(), sb, maxLength);
            }
        } else if (o instanceof OtpErlangBinary) {
            final OtpErlangBinary b = (OtpErlangBinary) o;
            final String s = new String(b.binaryValue(), Charsets.ISO_8859_1);
            sb.append(s);
        } else if (o != null) {
            sb.append(o.toString());
        }
        if (sb.length() > maxLength) {
            sb = new StringBuilder(sb.substring(0, maxLength));
            sb.append("... <truncated>");
        }
        return sb;
    }

    public static String normalizeSpaces(final String string) {
        return string.replaceAll("[\t\n\r ]+", " ");
    }

}
