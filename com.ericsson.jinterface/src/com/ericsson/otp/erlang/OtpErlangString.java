/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 *
 *     $Id$
 */
package com.ericsson.otp.erlang;

import java.io.Serializable;

/**
 * Provides a Java representation of Erlang strings.
 */
public class OtpErlangString extends OtpErlangObject implements Serializable,
	Cloneable {
    // don't change this!
    static final long serialVersionUID = -7053595217604929233L;

    private final String str;

    /**
     * Create an Erlang string from the given string.
     */
    public OtpErlangString(final String str) {
	this.str = str;
    }

    /**
     * Create an Erlang string from a list of integers
     * 
     * @throws OtpErlangDecodeException
     */
    public OtpErlangString(final OtpErlangList list)
	    throws OtpErlangDecodeException {
	str = list.asString();
    }

    /**
     * Create an Erlang string from a stream containing a string encoded in
     * Erlang external format.
     * 
     * @param buf
     *            the stream containing the encoded string.
     * 
     * @exception OtpErlangDecodeException
     *                if the buffer does not contain a valid external
     *                representation of an Erlang string.
     */
    public OtpErlangString(final OtpInputStream buf)
	    throws OtpErlangDecodeException {
	str = buf.read_string();
    }

    /**
     * Get the actual string contained in this object.
     * 
     * @return the raw string contained in this object, without regard to Erlang
     *         quoting rules.
     * 
     * @see #toString
     */
    public String stringValue() {
	return str;
    }

    /**
     * Get the printable version of the string contained in this object.
     * 
     * @return the string contained in this object, quoted.
     * 
     * @see #stringValue
     */

    @Override
    public String toString() {
	return "\"" + str + "\"";
    }

    /**
     * Convert this string to the equivalent Erlang external representation.
     * 
     * @param buf
     *            an output stream to which the encoded string should be
     *            written.
     */

    @Override
    public void encode(final OtpOutputStream buf) {
	buf.write_string(str);
    }

    /**
     * Determine if two strings are equal. They are equal if they represent the
     * same sequence of characters. This method can be used to compare
     * OtpErlangStrings with each other and with Strings.
     * 
     * @param o
     *            the OtpErlangString or String to compare to.
     * 
     * @return true if the strings consist of the same sequence of characters,
     *         false otherwise.
     */

    @Override
    public boolean equals(final Object o) {
	if (o instanceof String) {
	    return str.compareTo((String) o) == 0;
	} else if (o instanceof OtpErlangString) {
	    return str.compareTo(((OtpErlangString) o).str) == 0;
	}

	return false;
    }

    public static int[] stringToCodePoints(final String s) {
	final int n = s.length(), m = s.codePointCount(0, n);
	final int[] codePoints = new int[m];
	for (int i = 0, j = 0; i < n; ++j) {
	    final int codePoint = s.codePointAt(i);
	    codePoints[j] = codePoint;
	    i += Character.charCount(codePoint);
	}
	return codePoints;
    }
}
