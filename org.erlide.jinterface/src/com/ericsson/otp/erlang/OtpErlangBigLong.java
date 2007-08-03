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
 *     $Id: OtpErlangBigLong.java,v 1.5 2006/08/30 20:55:56 vladdu Exp $
 */
/*
 * Added BigInteger support -- Vlad Dumitrescu
 */
package com.ericsson.otp.erlang;

import java.io.Serializable;
import java.math.BigInteger;

/**
 * Provides a Java representation of Erlang large integral types.
 */
public class OtpErlangBigLong extends OtpErlangLong implements Serializable,
		Cloneable {

	// don't change this!
	private static final long serialVersionUID = 1610466859236755097L;

	private BigInteger val;

	/**
	 * Create an Erlang integer from the given value.
	 * 
	 * @param l
	 *            the long value to use.
	 */
	public OtpErlangBigLong(long l) {
		super(l);
	}

	/**
	 * Create an Erlang integer from the given value.
	 * 
	 * @param l
	 *            the BigInteger value to use.
	 */
	public OtpErlangBigLong(BigInteger l) {
		super(l.longValue());
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
	public OtpErlangBigLong(OtpInputStream buf) throws OtpErlangDecodeException {
		super(0);
		val = buf.read_biglong();
	}

	/**
	 * Get this number as a long.
	 * 
	 * @return the value of this number, as a long.
	 */
	@Override
	public long longValue() throws OtpErlangRangeException {
		final long l = val.longValue();

		if (BigInteger.valueOf(l).compareTo(val) != 0) {
			throw new OtpErlangRangeException("Value too large for long: "
					+ val);
		}

		return l;
	}

	/**
	 * Get this number as an int.
	 * 
	 * @return the value of this number, as an int.
	 * 
	 * @throws OtpErlangRangeException
	 *             if the value is too large to be represented as an int.
	 */
	@Override
	public int intValue() throws OtpErlangRangeException {
		final int i = val.intValue();

		if (BigInteger.valueOf(i).compareTo(val) != 0) {
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
	@Override
	public int uIntValue() throws OtpErlangRangeException {
		final int i = val.intValue();

		if (BigInteger.valueOf(i).compareTo(val) != 0) {
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
	@Override
	public short shortValue() throws OtpErlangRangeException {
		final short i = val.shortValue();

		if (BigInteger.valueOf(i).compareTo(val) != 0) {
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
	@Override
	public short uShortValue() throws OtpErlangRangeException {
		final short i = val.shortValue();

		if (BigInteger.valueOf(i).compareTo(val) != 0) {
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
	@Override
	public char charValue() throws OtpErlangRangeException {
		final char i = (char) val.shortValue();

		if (BigInteger.valueOf(i).compareTo(val) != 0) {
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
	@Override
	public byte byteValue() throws OtpErlangRangeException {
		final byte i = val.byteValue();

		if (BigInteger.valueOf(i).compareTo(val) != 0) {
			throw new OtpErlangRangeException("Value too large for byte: "
					+ val);
		}

		return i;
	}

	public BigInteger bigIntValue() {
		return val;
	}

	/**
	 * Get the string representation of this number.
	 * 
	 * @return the string representation of this number.
	 */
	@Override
	public String toString() {
		return val.toString();
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
		buf.write_biglong(val);
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
		if (!(o instanceof OtpErlangBigLong)) {
			return false;
		}

		final OtpErlangBigLong l = (OtpErlangBigLong) o;
		return this.bigIntValue() == l.bigIntValue();
	}

}
