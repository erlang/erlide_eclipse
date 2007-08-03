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
 *     $Id: OtpInputStream.java,v 1.11 2006/08/30 20:55:56 vladdu Exp $
 */
/*
 * Added BigInteger support -- Vlad Dumitrescu
 */
package com.ericsson.otp.erlang;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * Provides a stream for decoding Erlang terms from external format.
 * 
 * <p>
 * Note that this class is not synchronized, if you need synchronization you
 * must provide it yourself.
 */
public class OtpInputStream extends ByteArrayInputStream {

	/**
	 * Create a stream from a buffer containing encoded Erlang terms.
	 * 
	 * @param lbuf
	 */
	public OtpInputStream(byte[] lbuf) {
		super(lbuf);
	}

	/**
	 * Create a stream from a buffer containing encoded Erlang terms at the
	 * given offset and length.
	 * 
	 * @param lbuf
	 * @param offset
	 * @param length
	 */
	public OtpInputStream(byte[] lbuf, int offset, int length) {
		super(lbuf, offset, length);
	}

	/**
	 * Get the current position in the stream.
	 * 
	 * @return the current position in the stream.
	 */
	public int getPos() {
		return super.pos;
	}

	/**
	 * Set the current position in the stream.
	 * 
	 * @param lpos
	 *            the position to move to in the stream. If pos indicates a
	 *            position beyond the end of the stream, the position is move to
	 *            the end of the stream instead. If pos is negative, the
	 *            position is moved to the beginning of the stream instead.
	 * 
	 * @return the previous position in the stream.
	 */
	public int setPos(int lpos) {
		final int oldpos = super.pos;

		if (lpos > super.count) {
			lpos = super.count;
		} else if (lpos < 0) {
			lpos = 0;
		}

		super.pos = lpos;

		return oldpos;
	}

	/**
	 * Read an array of bytes from the stream. The method reads at most
	 * buf.length bytes from the input stream.
	 * 
	 * @param lbuf
	 * @return the number of bytes read.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next byte cannot be read.
	 */
	public int readN(byte[] lbuf) throws OtpErlangDecodeException {
		try {
			return super.read(lbuf);
		} catch (final IOException e) {
			throw new OtpErlangDecodeException("Cannot read from input stream");
		}
	}

	/**
	 * Look ahead one position in the stream without consuming the byte found
	 * there.
	 * 
	 * @return the next byte in the stream, as an integer.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next byte cannot be read.
	 */
	public int peek() throws OtpErlangDecodeException {
		int i;
		try {
			i = super.buf[super.pos];
			if (i < 0) {
				i += 256;
			}

			return i;
		} catch (final Exception e) {
			throw new OtpErlangDecodeException("Cannot read from input stream");
		}
	}

	/**
	 * Read a one byte integer from the stream.
	 * 
	 * @return the byte read, as an integer.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next byte cannot be read.
	 */
	public int read1() throws OtpErlangDecodeException {
		int i;
		i = super.read();

		if (i < 0) {
			throw new OtpErlangDecodeException("Cannot read from input stream");
		}

		return i;
	}

	/**
	 * Read a two byte big endian integer from the stream.
	 * 
	 * @return the bytes read, converted from big endian to an integer.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next byte cannot be read.
	 */
	public int read2BE() throws OtpErlangDecodeException {
		final byte[] b = new byte[2];
		try {
			super.read(b);
		} catch (final IOException e) {
			throw new OtpErlangDecodeException("Cannot read from input stream");
		}
		;
		return ((b[0] << 8) & 0xff00) + (b[1] & 0xff);
	}

	/**
	 * Read a four byte big endian integer from the stream.
	 * 
	 * @return the bytes read, converted from big endian to an integer.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next byte cannot be read.
	 */
	public int read4BE() throws OtpErlangDecodeException {
		final byte[] b = new byte[4];
		try {
			super.read(b);
		} catch (final IOException e) {
			throw new OtpErlangDecodeException("Cannot read from input stream");
		}
		;
		return ((b[0] << 24) & 0xff000000) + ((b[1] << 16) & 0xff0000)
				+ ((b[2] << 8) & 0xff00) + (b[3] & 0xff);
	}

	/**
	 * Read a two byte little endian integer from the stream.
	 * 
	 * @return the bytes read, converted from little endian to an integer.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next byte cannot be read.
	 */
	public int read2LE() throws OtpErlangDecodeException {
		final byte[] b = new byte[2];
		try {
			super.read(b);
		} catch (final IOException e) {
			throw new OtpErlangDecodeException("Cannot read from input stream");
		}
		;
		return ((b[1] << 8) & 0xff00) + (b[0] & 0xff);
	}

	/**
	 * Read a four byte little endian integer from the stream.
	 * 
	 * @return the bytes read, converted from little endian to an integer.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next byte cannot be read.
	 */
	public int read4LE() throws OtpErlangDecodeException {
		final byte[] b = new byte[4];
		try {
			super.read(b);
		} catch (final IOException e) {
			throw new OtpErlangDecodeException("Cannot read from input stream");
		}
		;
		return ((b[3] << 24) & 0xff000000) + ((b[2] << 16) & 0xff0000)
				+ ((b[1] << 8) & 0xff00) + (b[0] & 0xff);
	}

	/**
	 * Read a little endian integer from the stream.
	 * 
	 * @param n
	 *            the number of bytes to read
	 * 
	 * @return the bytes read, converted from little endian to an integer.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next byte cannot be read.
	 */
	public long readLE(int n) throws OtpErlangDecodeException {
		final byte[] b = new byte[n];
		try {
			super.read(b);
		} catch (final IOException e) {
			throw new OtpErlangDecodeException("Cannot read from input stream");
		}
		;
		long v = 0;
		for (int i = n - 1; i >= 0; i--) {
			v = (v << 8) | ((long) b[i] & 0xff);
		}
		return v;
	}

	public BigInteger readBigLE(int n) throws OtpErlangDecodeException {
		final byte[] b = new byte[n];
		try {
			super.read(b);
		} catch (final IOException e) {
			throw new OtpErlangDecodeException("Cannot read from input stream");
		}
		;
		BigInteger v = BigInteger.ZERO;
		for (int i = n - 1; i >= 0; i--) {
			v = v.multiply(BigInteger.valueOf(256)).add(
					BigInteger.valueOf((long) b[i] & 0xff));
		}
		return v;
	}

	/**
	 * Read an Erlang atom from the stream and interpret the value as a boolean.
	 * 
	 * @return true if the atom at the current position in the stream contains
	 *         the value 'true' (ignoring case), false otherwise.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next term in the stream is not an atom.
	 */
	public boolean read_boolean() throws OtpErlangDecodeException {
		return Boolean.valueOf(this.read_atom()).booleanValue();
	}

	/**
	 * Read an Erlang atom from the stream.
	 * 
	 * @return a String containing the value of the atom.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next term in the stream is not an atom.
	 */
	public String read_atom() throws OtpErlangDecodeException {
		int tag;
		int len;
		byte[] strbuf;
		String atom;

		tag = this.read1();
		if (tag == OtpExternal.versionTag) {
			tag = this.read1();
		}

		if (tag != OtpExternal.atomTag) {
			throw new OtpErlangDecodeException(
					"wrong tag encountered, expected " + OtpExternal.atomTag
							+ ", got " + tag);
		}

		len = this.read2BE();

		strbuf = new byte[len];
		this.readN(strbuf);
		atom = new String(strbuf);

		if (atom.length() > OtpExternal.maxAtomLength) {
			atom = atom.substring(0, OtpExternal.maxAtomLength);
		}

		return atom;
	}

	/**
	 * Read an Erlang binary from the stream.
	 * 
	 * @return a byte array containing the value of the binary.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next term in the stream is not a binary.
	 */
	public byte[] read_binary() throws OtpErlangDecodeException {
		int tag;
		int len;
		byte[] bin;

		tag = this.read1();
		if (tag == OtpExternal.versionTag) {
			tag = this.read1();
		}

		if (tag != OtpExternal.binTag) {
			throw new OtpErlangDecodeException(
					"Wrong tag encountered, expected " + OtpExternal.binTag
							+ ", got " + tag);
		}

		len = this.read4BE();

		bin = new byte[len];
		this.readN(bin);

		return bin;
	}

	/**
	 * Read an Erlang float from the stream.
	 * 
	 * @return the float value.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next term in the stream is not a float.
	 */
	public float read_float() throws OtpErlangDecodeException {
		final BigDecimal val = getFloatOrDouble();

		return val.floatValue();

		/*
		 * 
		 * double d = this.read_double(); float f = (float) d;
		 * 
		 * if (java.lang.Math.abs(d - f) >= 1.0E-20) throw new
		 * OtpErlangDecodeException("Value cannot be represented as float: " +
		 * d);
		 * 
		 * return f;
		 */
	}

	/**
	 * Read an Erlang float from the stream.
	 * 
	 * @return the float value, as a double.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next term in the stream is not a float.
	 */
	public double read_double() throws OtpErlangDecodeException {
		final BigDecimal val = getFloatOrDouble();

		return val.doubleValue();
	}

	private BigDecimal getFloatOrDouble() throws OtpErlangDecodeException {
		BigDecimal val;
		int epos;
		int exp;
		final byte[] strbuf = new byte[31];
		String str;
		int tag;

		// parse the stream
		tag = this.read1();
		if (tag == OtpExternal.versionTag) {
			tag = this.read1();
		}

		if (tag != OtpExternal.floatTag) {
			throw new OtpErlangDecodeException(
					"Wrong tag encountered, expected " + OtpExternal.floatTag
							+ ", got " + tag);
		}

		// get the string
		this.readN(strbuf);
		str = new String(strbuf);

		// find the exponent prefix 'e' in the string
		epos = str.indexOf('e', 0);

		if (epos < 0) {
			throw new OtpErlangDecodeException("Invalid float format: '" + str
					+ "'");
		}

		// remove the sign from the exponent, if positive
		String estr = str.substring(epos + 1).trim();

		if ("+".equals(estr.substring(0, 1))) {
			estr = estr.substring(1);
		}

		// now put the mantissa and exponent together
		exp = Integer.valueOf(estr).intValue();
		val = new BigDecimal(str.substring(0, epos)).movePointRight(exp);

		return val;
	}

	/**
	 * Read one byte from the stream.
	 * 
	 * @return the byte read.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next byte cannot be read.
	 */
	public byte read_byte() throws OtpErlangDecodeException {
		final long l = this.read_long(false);
		final byte i = (byte) l;

		if (l != i) {
			throw new OtpErlangDecodeException("Value does not fit in byte: "
					+ l);
		}

		return i;
	}

	/**
	 * Read a character from the stream.
	 * 
	 * @return the character value.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next term in the stream is not an integer that can be
	 *             represented as a char.
	 */
	public char read_char() throws OtpErlangDecodeException {
		final long l = this.read_long(true);
		final char i = (char) l;

		if (l != (i & 0xffffL)) {
			throw new OtpErlangDecodeException("Value does not fit in char: "
					+ l);
		}

		return i;
	}

	/**
	 * Read an unsigned integer from the stream.
	 * 
	 * @return the integer value.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next term in the stream can not be represented as a
	 *             positive integer.
	 */
	public int read_uint() throws OtpErlangDecodeException {
		final long l = this.read_long(true);
		final int i = (int) l;

		if (l != (i & 0xFFFFffffL)) {
			throw new OtpErlangDecodeException("Value does not fit in uint: "
					+ l);
		}

		return i;
	}

	/**
	 * Read an integer from the stream.
	 * 
	 * @return the integer value.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next term in the stream can not be represented as an
	 *             integer.
	 */
	public int read_int() throws OtpErlangDecodeException {
		final long l = this.read_long(false);
		final int i = (int) l;

		if (l != i) {
			throw new OtpErlangDecodeException("Value does not fit in int: "
					+ l);
		}

		return i;
	}

	/**
	 * Read an unsigned short from the stream.
	 * 
	 * @return the short value.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next term in the stream can not be represented as a
	 *             positive short.
	 */
	public short read_ushort() throws OtpErlangDecodeException {
		final long l = this.read_long(true);
		final short i = (short) l;

		if (l != (i & 0xffffL)) {
			throw new OtpErlangDecodeException("Value does not fit in ushort: "
					+ l);
		}

		return i;
	}

	/**
	 * Read a short from the stream.
	 * 
	 * @return the short value.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the next term in the stream can not be represented as a
	 *                short.
	 */
	public short read_short() throws OtpErlangDecodeException {
		final long l = this.read_long(false);
		final short i = (short) l;

		if (l != i) {
			throw new OtpErlangDecodeException("Value does not fit in short: "
					+ l);
		}

		return i;
	}

	/**
	 * Read an unsigned long from the stream.
	 * 
	 * @return the long value.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next term in the stream can not be represented as a
	 *             positive long.
	 */
	public long read_ulong() throws OtpErlangDecodeException {
		return this.read_long(true);
	}

	/**
	 * Read a long from the stream.
	 * 
	 * @return the long value.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next term in the stream can not be represented as a
	 *             long.
	 */
	public long read_long() throws OtpErlangDecodeException {
		return this.read_long(false);
	}

	public long read_long(boolean unsigned) throws OtpErlangDecodeException {
		int tag;
		int sign;
		int arity;
		long val;

		tag = this.read1();
		if (tag == OtpExternal.versionTag) {
			tag = this.read1();
		}

		switch (tag) {
		case OtpExternal.smallIntTag:
			val = this.read1() & 0xffL;
			break;

		case OtpExternal.intTag:
			val = this.read4BE();
			if (unsigned && val < 0) {
				throw new OtpErlangDecodeException("Value not unsigned: " + val);
			}
			break;

		case OtpExternal.smallBigTag:
			arity = this.read1();

			if (arity > 8) {
				throw new OtpErlangDecodeException(
						"Arity for smallBig may not be more than 8, was "
								+ arity);
			}

			sign = this.read1();

			// obs! little endian here
			val = this.readLE(arity);
			if (unsigned) {
				if (sign != 0) {
					throw new OtpErlangDecodeException(
							"Value not unsigned, val " + val + " sign " + sign);
				}
			} else if ((val == -val) ? sign == 0 : val < 0) {
				throw new OtpErlangDecodeException(
						"Value of smallBig does not fit in long, val " + val
								+ " sign " + sign);
			} else if (sign != 0) {
				val = -val;
			}
			break;

		case OtpExternal.largeBigTag:
		default:
			throw new OtpErlangDecodeException("Not valid integer tag: " + tag);
		}

		return val;
	}

	public BigInteger read_biglong() throws OtpErlangDecodeException {
		int tag;
		int sign;
		int arity;
		BigInteger val;

		tag = this.read1();
		if (tag == OtpExternal.versionTag) {
			tag = this.read1();
		}

		switch (tag) {
		case OtpExternal.smallBigTag:
			arity = this.read1();
			sign = this.read1();

			// obs! little endian here
			val = this.readBigLE(arity);
			if (sign != 0) {
				val = BigInteger.ZERO.subtract(val);
			}
			break;
		case OtpExternal.largeBigTag:
			arity = this.read4LE();
			sign = this.read1();

			// obs! little endian here
			val = this.readBigLE(arity);
			if (sign != 0) {
				val = BigInteger.ZERO.subtract(val);
			}
			break;
		default:
			throw new OtpErlangDecodeException("Not valid big integer tag: "
					+ tag);
		}
		return val;
	}

	/**
	 * Read a list header from the stream.
	 * 
	 * @return the arity of the list.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next term in the stream is not a list.
	 */
	public int read_list_head() throws OtpErlangDecodeException {
		int arity = 0;
		int tag = this.read1();

		if (tag == OtpExternal.versionTag) {
			tag = this.read1();
		}

		switch (tag) {
		case OtpExternal.nilTag:
			arity = 0;
			break;

		case OtpExternal.stringTag:
			arity = this.read2BE();
			break;

		case OtpExternal.listTag:
			arity = this.read4BE();
			break;

		default:
			throw new OtpErlangDecodeException("Not valid list tag: " + tag);
		}

		return arity;
	}

	/**
	 * Read a tuple header from the stream.
	 * 
	 * @return the arity of the tuple.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next term in the stream is not a tuple.
	 */
	public int read_tuple_head() throws OtpErlangDecodeException {
		int arity = 0;
		int tag = this.read1();

		if (tag == OtpExternal.versionTag) {
			tag = this.read1();
		}

		// decode the tuple header and get arity
		switch (tag) {
		case OtpExternal.smallTupleTag:
			arity = this.read1();
			break;

		case OtpExternal.largeTupleTag:
			arity = this.read4BE();
			break;

		default:
			throw new OtpErlangDecodeException("Not valid tuple tag: " + tag);
		}

		return arity;
	}

	/**
	 * Read an empty list from the stream.
	 * 
	 * @return zero (the arity of the list).
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next term in the stream is not an empty list.
	 */
	public int read_nil() throws OtpErlangDecodeException {
		int arity = 0;
		int tag = this.read1();

		if (tag == OtpExternal.versionTag) {
			tag = this.read1();
		}

		switch (tag) {
		case OtpExternal.nilTag:
			arity = 0;
			break;

		default:
			throw new OtpErlangDecodeException("Not valid nil tag: " + tag);
		}

		return arity;
	}

	/**
	 * Read an Erlang PID from the stream.
	 * 
	 * @return the value of the PID.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next term in the stream is not an Erlang PID.
	 */
	public OtpErlangPid read_pid() throws OtpErlangDecodeException {
		String node;
		int id;
		int serial;
		int creation;
		int tag;

		tag = this.read1();
		if (tag == OtpExternal.versionTag) {
			tag = this.read1();
		}

		if (tag != OtpExternal.pidTag) {
			throw new OtpErlangDecodeException(
					"Wrong tag encountered, expected " + OtpExternal.pidTag
							+ ", got " + tag);
		}

		node = this.read_atom();
		id = this.read4BE() & 0x7fff; // 15 bits
		serial = this.read4BE() & 0x07; // 3 bits
		creation = this.read1() & 0x03; // 2 bits

		return new OtpErlangPid(node, id, serial, creation);
	}

	/**
	 * Read an Erlang port from the stream.
	 * 
	 * @return the value of the port.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next term in the stream is not an Erlang port.
	 */
	public OtpErlangPort read_port() throws OtpErlangDecodeException {
		String node;
		int id;
		int creation;
		int tag;

		tag = this.read1();
		if (tag == OtpExternal.versionTag) {
			tag = this.read1();
		}

		if (tag != OtpExternal.portTag) {
			throw new OtpErlangDecodeException(
					"Wrong tag encountered, expected " + OtpExternal.portTag
							+ ", got " + tag);
		}

		node = this.read_atom();
		id = this.read4BE() & 0x3ffff; // 18 bits
		creation = this.read1() & 0x03; // 2 bits

		return new OtpErlangPort(node, id, creation);
	}

	/**
	 * Read an Erlang reference from the stream.
	 * 
	 * @return the value of the reference
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next term in the stream is not an Erlang reference.
	 */
	public OtpErlangRef read_ref() throws OtpErlangDecodeException {
		String node;
		int id;
		int creation;
		int tag;

		tag = this.read1();
		if (tag == OtpExternal.versionTag) {
			tag = this.read1();
		}

		switch (tag) {
		case OtpExternal.refTag:
			node = this.read_atom();
			id = this.read4BE() & 0x3ffff; // 18 bits
			creation = this.read1() & 0x03; // 2 bits
			return new OtpErlangRef(node, id, creation);

		case OtpExternal.newRefTag:
			final int arity = this.read2BE();
			node = this.read_atom();
			creation = this.read1() & 0x03; // 2 bits

			final int[] ids = new int[arity];
			for (int i = 0; i < arity; i++) {
				ids[i] = this.read4BE();
			}
			ids[0] &= 0x3ffff; // first id gets truncated to 18 bits
			return new OtpErlangRef(node, ids, creation);

		default:
			throw new OtpErlangDecodeException(
					"Wrong tag encountered, expected ref, got " + tag);
		}
	}

	/**
	 * Read a string from the stream.
	 * 
	 * @return the value of the string.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the next term in the stream is not a string.
	 */
	public String read_string() throws OtpErlangDecodeException {
		int tag;
		int len;
		byte[] strbuf;
		char[] charbuf;

		tag = this.read1();
		if (tag == OtpExternal.versionTag) {
			tag = this.read1();
		}

		switch (tag) {

		case OtpExternal.stringTag:
			len = this.read2BE();
			strbuf = new byte[len];
			this.readN(strbuf);
			return new String(strbuf);

		case OtpExternal.nilTag:
			return "";

		case OtpExternal.listTag: // List when unicode +
			len = this.read4BE();
			charbuf = new char[len];

			for (int i = 0; i < len; i++) {
				charbuf[i] = this.read_char();
			}

			this.read_nil();
			return new String(charbuf);

		default:
			throw new OtpErlangDecodeException(
					"Wrong tag encountered, expected " + OtpExternal.stringTag
							+ " or " + OtpExternal.listTag + ", got " + tag);
		}
	}

	/**
	 * Read an arbitrary Erlang term from the stream.
	 * 
	 * @return the Erlang term.
	 * 
	 * @throws OtpErlangDecodeException
	 *             if the stream does not contain a known Erlang type at the
	 *             next position.
	 */
	public OtpErlangObject read_any() throws OtpErlangDecodeException {
		// calls one of the above functions, depending on o
		int tag = this.peek();
		if (tag == OtpExternal.versionTag) {
			this.read1();
			tag = this.peek();
		}

		switch (tag) {
		case OtpExternal.smallIntTag:
		case OtpExternal.intTag:
			return new OtpErlangLong(this);

		case OtpExternal.smallBigTag:
		case OtpExternal.largeBigTag:
			return new OtpErlangBigLong(this);

		case OtpExternal.atomTag:
			return new OtpErlangAtom(this);

		case OtpExternal.floatTag:
			return new OtpErlangDouble(this);

		case OtpExternal.refTag:
		case OtpExternal.newRefTag:
			return new OtpErlangRef(this);

		case OtpExternal.portTag:
			return new OtpErlangPort(this);

		case OtpExternal.pidTag:
			return new OtpErlangPid(this);

		case OtpExternal.stringTag:
			return new OtpErlangString(this);

		case OtpExternal.listTag:
		case OtpExternal.nilTag:
			return new OtpErlangList(this);

		case OtpExternal.smallTupleTag:
		case OtpExternal.largeTupleTag:
			return new OtpErlangTuple(this);

		case OtpExternal.binTag:
			return new OtpErlangBinary(this);

		default:
			throw new OtpErlangDecodeException("Uknown data type: " + tag);
		}
	}
}
