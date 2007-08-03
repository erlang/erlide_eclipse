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
 *     $Id: OtpErlangLong.java,v 1.7 2006/08/30 20:55:56 vladdu Exp $
 */
package com.ericsson.otp.erlang;

import java.io.Serializable;

/**
 * Provides a Java representation of Erlang integral types. Erlang does not
 * distinguish between different integral types, however this class and its
 * subclasses {@link OtpErlangByte},{@linkOtpErlangChar},
 * {@link OtpErlangInt}, and {@link OtpErlangShort}attempt to map the Erlang
 * types onto the various Java integral types. Two additional classes,
 * {@link OtpErlangUInt}and {@linkOtpErlangUShort} are provided for Corba
 * compatibility. See the documentation for IC for more information.
 */
public class OtpErlangLong extends OtpErlangObject implements Serializable,
		Cloneable {

	// don't change this!
	static final long serialVersionUID = 1610466859236755096L;

	private long val;

	/**
	 * Create an Erlang integer from the given value.
	 * 
	 * @param l
	 *            the long value to use.
	 */
	public OtpErlangLong(long l) {
		val = l;
	}

	/**
	 * Create an Erlang integer from a stream containing an integer encoded in
	 * Erlang external format.
	 * 
	 * @param buf
	 *            the stream containing the encoded value.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the buffer does not contain a valid external
	 *             representation of an Erlang integer.
	 */
	public OtpErlangLong(OtpInputStream buf) throws OtpErlangDecodeException {
		val = buf.read_long();
	}

	/**
	 * Get this number as a long.
	 * 
	 * @return the value of this number, as a long.
	 * @throws OtpErlangRangeException
	 */
	public long longValue() throws OtpErlangRangeException {
		return val;
	}

	/**
	 * Get this number as an int.
	 * 
	 * @return the value of this number, as an int.
	 * 
	 * @throws OtpErlangRangeException
	 *             if the value is too large to be represented as an int.
	 */
	public int intValue() throws OtpErlangRangeException {
		final int i = (int) val;

		if (i != val) {
			throw new OtpErlangRangeException("Value too large for int: " + val);
		}

		return i;
	}

	/**
	 * Get this number as a non-negative int.
	 * 
	 * @return the value of this number, as an int.
	 * 
	 * @throws OtpErlangRangeException
	 *             if the value is too large to be represented as an int, or if
	 *             the value is negative.
	 */
	public int uIntValue() throws OtpErlangRangeException {
		final int i = (int) val;

		if (i != val) {
			throw new OtpErlangRangeException("Value too large for int: " + val);
		} else if (i < 0) {
			throw new OtpErlangRangeException("Value not positive: " + val);
		}

		return i;
	}

	/**
	 * Get this number as a short.
	 * 
	 * @return the value of this number, as a short.
	 * 
	 * @throws OtpErlangRangeException
	 *             if the value is too large to be represented as a short.
	 */
	public short shortValue() throws OtpErlangRangeException {
		final short i = (short) val;

		if (i != val) {
			throw new OtpErlangRangeException("Value too large for short: "
					+ val);
		}

		return i;
	}

	/**
	 * Get this number as a non-negative short.
	 * 
	 * @return the value of this number, as a short.
	 * 
	 * @throws OtpErlangRangeException
	 *             if the value is too large to be represented as a short, or if
	 *             the value is negative.
	 */
	public short uShortValue() throws OtpErlangRangeException {
		final short i = (short) val;

		if (i != val) {
			throw new OtpErlangRangeException("Value too large for short: "
					+ val);
		} else if (i < 0) {
			throw new OtpErlangRangeException("Value not positive: " + val);
		}

		return i;
	}

	/**
	 * Get this number as a char.
	 * 
	 * @return the char value of this number.
	 * 
	 * @throws OtpErlangRangeException
	 *             if the value is too large to be represented as a char.
	 */
	public char charValue() throws OtpErlangRangeException {
		final char i = (char) val;

		if (i != val) {
			throw new OtpErlangRangeException("Value too large for char: "
					+ val);
		}

		return i;
	}

	/**
	 * Get this number as a byte.
	 * 
	 * @return the byte value of this number.
	 * 
	 * @throws OtpErlangRangeException
	 *             if the value is too large to be represented as a byte.
	 */
	public byte byteValue() throws OtpErlangRangeException {
		final byte i = (byte) val;

		if (i != val) {
			throw new OtpErlangRangeException("Value too large for byte: "
					+ val);
		}

		return i;
	}

	/**
	 * Get the string representation of this number.
	 * 
	 * @return the string representation of this number.
	 */
	@Override
	public String toString() {
		return "" + val;
	}

	/**
	 * Convert this number to the equivalent Erlang external representation.
	 * 
	 * @param buf
	 *            an output stream to which the encoded number should be
	 *            written.
	 */
	@Override
	public void encode(OtpOutputStream buf) {
		buf.write_long(val);
	}

	/**
	 * Determine if two numbers are equal. Numbers are equal if they contain the
	 * same value.
	 * 
	 * @param o
	 *            the number to compare to.
	 * 
	 * @return true if the numbers have the same value.
	 */
	@Override
	public boolean equals(Object o) {
		if (!(o instanceof OtpErlangLong)) {
			return false;
		}

		final OtpErlangLong l = (OtpErlangLong) o;
		return val == l.val;
	}
}