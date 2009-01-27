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
import java.math.BigInteger;

/**
 * Provides a Java representation of Erlang integral types. Erlang does not
 * distinguish between different integral types, however this class and its
 * subclasses {@link OtpErlangByte}, {@link OtpErlangChar}, {@link OtpErlangInt}
 * , and {@link OtpErlangShort} attempt to map the Erlang types onto the various
 * Java integral types. Two additional classes, {@link OtpErlangUInt} and
 * {@link OtpErlangUShort} are provided for Corba compatibility. See the
 * documentation for IC for more information.
 **/
public class OtpErlangLong extends OtpErlangObject implements Serializable,
		Cloneable {
	// don't change this!
	static final long serialVersionUID = 1610466859236755096L;

	private long val;
	private BigInteger bigVal = null;

	/**
	 * Create an Erlang integer from the given value.
	 * 
	 * @param l
	 *            the long value to use.
	 **/
	public OtpErlangLong(long l) {
		this.val = l;
	}

	/**
	 * Create an Erlang integer from the given value.
	 * 
	 * @param val
	 *            the long value to use.
	 **/
	public OtpErlangLong(BigInteger v) {
		if (v == null) {
			throw new java.lang.NullPointerException();
		}
		if (v.bitLength() < 64) {
			this.val = v.longValue();
		} else {
			this.bigVal = v;
		}
	}

	/**
	 * Create an Erlang integer from a stream containing an integer encoded in
	 * Erlang external format.
	 * 
	 * @param buf
	 *            the stream containing the encoded value.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the buffer does not contain a valid external
	 *                representation of an Erlang integer.
	 **/
	public OtpErlangLong(OtpInputStream buf) throws OtpErlangDecodeException {
		byte[] b = buf.read_integer_byte_array();
		try {
			this.val = OtpInputStream.byte_array_to_long(b, false);
		} catch (OtpErlangDecodeException e) {
			this.bigVal = new BigInteger(b);
		}
	}

	/**
	 * Get this number as a BigInteger.
	 * 
	 * @return the value of this number, as a BigInteger.
	 **/
	public BigInteger bigIntegerValue() {
		if (this.bigVal != null) {
			return this.bigVal;
		} else {
			return BigInteger.valueOf(val);
		}
	}

	/**
	 * Get this number as a long, or rather truncate all but the least
	 * significant 64 bits from the 2's complement representation of this number
	 * and return them as a long.
	 * 
	 * @return the value of this number, as a long.
	 **/
	public long longValue() {
		if (this.bigVal != null) {
			return this.bigVal.longValue();
		} else {
			return this.val;
		}
	}

	/**
	 * Determine if this value can be represented as a long without truncation.
	 * 
	 * @return true if this value fits in a long, false otherwise.
	 **/
	public boolean isLong() {
		// To just chech this.bigVal is a wee bit to simple, since
		// there just might have be a mean bignum that arrived on
		// a stream, and was a long disguised as more than 8 byte integer.
		if (this.bigVal != null) {
			return this.bigVal.bitLength() < 64;
		}
		return true;
	}

	/**
	 * Determine if this value can be represented as an unsigned long without
	 * truncation, that is if the value is non-negative and its bit pattern
	 * completely fits in a long.
	 * 
	 * @return true if this value is non-negative and fits in a long false
	 *         otherwise.
	 **/
	public boolean isULong() {
		// Here we have the same problem as for isLong(), plus
		// the whole range 1<<63 .. (1<<64-1) is allowed.
		if (this.bigVal != null) {
			return this.bigVal.signum() >= 0 && this.bigVal.bitLength() <= 64;
		}
		return this.val >= 0;
	}

	/**
	 * Returns the number of bits in the minimal two's-complement representation
	 * of this BigInteger, excluding a sign bit.
	 * 
	 * @return number of bits in the minimal two's-complement representation of
	 *         this BigInteger, excluding a sign bit.
	 **/
	public int bitLength() {
		if (this.bigVal != null) {
			return this.bigVal.bitLength();
		}
		if (this.val == 0 || this.val == -1) {
			return 0;
		} else {
			// Binary search for bit length
			int i = 32; // mask length
			long m = (1L << i) - 1; // AND mask with ones in little end
			if (this.val < 0) {
				m = ~m; // OR mask with ones in big end
				for (int j = i >> 1; j > 0; j >>= 1) { // mask delta
					if ((this.val | m) == this.val) { // mask >= enough
						i -= j;
						m >>= j; // try less bits
					} else {
						i += j;
						m <<= j; // try more bits
					}
				}
				if ((this.val | m) != this.val)
					i++; // mask < enough
			} else {
				for (int j = i >> 1; j > 0; j >>= 1) { // mask delta
					if ((this.val & m) == this.val) { // mask >= enough
						i -= j;
						m >>= j; // try less bits
					} else {
						i += j;
						m = (m << j) | m; // try more bits
					}
				}
				if ((this.val & m) != this.val)
					i++; // mask < enough
			}
			return i;
		}
	}

	/**
	 * Return the signum function of this object.
	 * 
	 * @return -1, 0 or 1 as the value is negative, zero or positive.
	 **/
	public int signum() {
		if (this.bigVal != null) {
			return this.bigVal.signum();
		} else {
			return this.val > 0 ? 1 : (this.val < 0 ? -1 : 0);
		}
	}

	/**
	 * Get this number as an int.
	 * 
	 * @return the value of this number, as an int.
	 * 
	 * @exception OtpErlangRangeException
	 *                if the value is too large to be represented as an int.
	 **/
	public int intValue() throws OtpErlangRangeException {
		long l = this.longValue();
		int i = (int) l;

		if (i != l) {
			throw new OtpErlangRangeException("Value too large for int: " + val);
		}

		return i;
	}

	/**
	 * Get this number as a non-negative int.
	 * 
	 * @return the value of this number, as an int.
	 * 
	 * @exception OtpErlangRangeException
	 *                if the value is too large to be represented as an int, or
	 *                if the value is negative.
	 **/
	public int uIntValue() throws OtpErlangRangeException {
		long l = this.longValue();
		int i = (int) l;

		if (i != l) {
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
	 * @exception OtpErlangRangeException
	 *                if the value is too large to be represented as a short.
	 **/
	public short shortValue() throws OtpErlangRangeException {
		long l = this.longValue();
		short i = (short) l;

		if (i != l) {
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
	 * @exception OtpErlangRangeException
	 *                if the value is too large to be represented as a short, or
	 *                if the value is negative.
	 **/
	public short uShortValue() throws OtpErlangRangeException {
		long l = this.longValue();
		short i = (short) l;

		if (i != l) {
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
	 * @exception OtpErlangRangeException
	 *                if the value is too large to be represented as a char.
	 **/
	public char charValue() throws OtpErlangRangeException {
		long l = this.longValue();
		char i = (char) l;

		if (i != l) {
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
	 * @exception OtpErlangRangeException
	 *                if the value is too large to be represented as a byte.
	 **/
	public byte byteValue() throws OtpErlangRangeException {
		long l = this.longValue();
		byte i = (byte) l;

		if (i != l) {
			throw new OtpErlangRangeException("Value too large for byte: "
					+ val);
		}

		return i;
	}

	/**
	 * Get the string representation of this number.
	 * 
	 * @return the string representation of this number.
	 **/
	@Override
	public String toString() {
		if (bigVal != null) {
			return "" + bigVal;
		} else {
			return "" + val;
		}
	}

	/**
	 * Convert this number to the equivalent Erlang external representation.
	 * 
	 * @param buf
	 *            an output stream to which the encoded number should be
	 *            written.
	 **/
	@Override
	public void encode(OtpOutputStream buf) {
		if (this.bigVal != null) {
			buf.write_big_integer(this.bigVal);
		} else {
			buf.write_long(this.val);
		}
	}

	/**
	 * Determine if two numbers are equal. Numbers are equal if they contain the
	 * same value.
	 * 
	 * @param o
	 *            the number to compare to.
	 * 
	 * @return true if the numbers have the same value.
	 **/
	@Override
	public boolean equals(Object o) {
		if (!(o instanceof OtpErlangLong))
			return false;

		OtpErlangLong that = (OtpErlangLong) o;

		if (this.bigVal != null && that.bigVal != null) {
			return (this.bigVal.equals(that.bigVal));
		} else if (this.bigVal == null && that.bigVal == null) {
			return this.val == that.val;
		}
		return false;
	}
}
