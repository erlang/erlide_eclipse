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
package org.erlide.util;

import java.io.IOException;
import java.io.InputStream;
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
import java.util.Scanner;
import java.util.StringTokenizer;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangInt;
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
    private static final String BUNDLE_NAME = "org.erlide.util.util"; //$NON-NLS-1$
    private static final char[] DOUBLE_QUOTES = "''".toCharArray(); //$NON-NLS-1$
    private static final String EMPTY_ARGUMENT = "   "; //$NON-NLS-1$
    // public static final String[] fgEmptyStringArray = new String[0];
    private static final char[] SINGLE_QUOTE = "'".toCharArray(); //$NON-NLS-1$

    /* Bundle containing messages */
    private static ResourceBundle bundle;

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
                if ((start = CharOperation.indexOf('}', messageWithNoDoubleQuotes,
                        end + 1)) > -1) {
                    int index = -1;
                    final String argId = new String(messageWithNoDoubleQuotes, end + 1,
                            start - end - 1);
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
                            output.append(messageWithNoDoubleQuotes, end + 1, start - end);
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
    public static String[] getProblemArgumentsFromMarker(final String argumentsString) {
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
        final String argumentsString1 = argumentsString.substring(index + 1, length);

        String[] args = new String[length];
        int count = 0;

        final StringTokenizer tokenizer = new StringTokenizer(argumentsString1,
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
     * Creates a NLS catalog for the given locale.
     */
    public static void relocalize() {
        try {
            bundle = ResourceBundle.getBundle(BUNDLE_NAME, Locale.getDefault());
        } catch (final MissingResourceException e) {
            ErlLogger
                    .error("Missing resource : " + BUNDLE_NAME.replace('.', '/') + ".properties for locale " + Locale.getDefault()); //$NON-NLS-1$//$NON-NLS-2$
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
                ErlLogger.error("bad binary value in stringValue" + " (can't decode): "
                        + o);
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

    public static String ioListToString(final OtpErlangObject o, final int maxLength) {
        StringBuilder sb = new StringBuilder();
        sb = ioListToStringBuilder(o, sb, maxLength);
        return sb.toString();
    }

    public static String getInputStreamAsString(final InputStream is,
            final String encoding) {
        final Scanner s = new Scanner(is, encoding).useDelimiter("\\A");
        return s.hasNext() ? s.next() : "";
    }

    private static StringBuilder ioListToStringBuilder(final OtpErlangObject o,
            final StringBuilder sb0, final int maxLength) {
        StringBuilder sb = sb0;
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
            String s = decode(b.binaryValue(), Charsets.UTF_8);
            if (s == null) {
                s = new String(b.binaryValue(), Charsets.ISO_8859_1);
            }
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

    public static int getIntegerValue(final OtpErlangObject object, final int defaultValue) {
        if (object instanceof OtpErlangLong) {
            final OtpErlangLong l = (OtpErlangLong) object;
            try {
                return l.intValue();
            } catch (final OtpErlangRangeException e) {
            }
        }
        if (object instanceof OtpErlangInt) {
            final OtpErlangInt l = (OtpErlangInt) object;
            try {
                return l.intValue();
            } catch (final OtpErlangRangeException e) {
            }
        }
        return defaultValue;
    }

    public static OtpErlangList listValue(final OtpErlangObject o) {
        if (o instanceof OtpErlangList) {
            return (OtpErlangList) o;
        } else if (o instanceof OtpErlangString) {
            final OtpErlangString erlangString = (OtpErlangString) o;
            final int[] codePoints = OtpErlangString.stringToCodePoints(erlangString
                    .stringValue());
            final OtpErlangObject elements[] = new OtpErlangObject[codePoints.length];
            for (int i = 0; i < codePoints.length; i++) {
                elements[i] = new OtpErlangLong(codePoints[i]);
            }
            return new OtpErlangList(elements);
        }
        return null;
    }

}
