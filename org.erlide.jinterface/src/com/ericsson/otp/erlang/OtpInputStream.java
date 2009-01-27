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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.math.BigDecimal;

/**
 * Provides a stream for decoding Erlang terms from external format.
 * 
 * <p>
 * Note that this class is not synchronized, if you need synchronization you
 * must provide it yourself.
 **/
public class OtpInputStream extends ByteArrayInputStream {
	/**
	 * Create a stream from a buffer containing encoded Erlang terms.
	 **/
	public OtpInputStream(byte[] buf) {
		super(buf);
	}

	/**
	 * Create a stream from a buffer containing encoded Erlang terms at the
	 * given offset and length.
	 **/
	public OtpInputStream(byte[] buf, int offset, int length) {
		super(buf, offset, length);
	}

	/**
	 * Get the current position in the stream.
	 * 
	 * @return the current position in the stream.
	 **/
	public int getPos() {
		return super.pos;
	}

	/**
	 * Set the current position in the stream.
	 * 
	 * @param pos
	 *            the position to move to in the stream. If pos indicates a
	 *            position beyond the end of the stream, the position is move to
	 *            the end of the stream instead. If pos is negative, the
	 *            position is moved to the beginning of the stream instead.
	 * 
	 * @return the previous position in the stream.
	 **/
	public int setPos(int pos) {
		int oldpos = super.pos;

		if (pos > super.count) {
			pos = super.count;
		} else if (pos < 0) {
			pos = 0;
		}

		super.pos = pos;

		return oldpos;
	}

	/**
	 * Read an array of bytes from the stream. The method reads at most
	 * buf.length bytes from the input stream.
	 * 
	 * @return the number of bytes read.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the next byte cannot be read.
	 **/
	public int readN(byte[] buf) throws OtpErlangDecodeException {
		return this.readN(buf, 0, buf.length);
	}

	/**
	 * Read an array of bytes from the stream. The method reads at most len
	 * bytes from the input stream into offset off of the buffer.
	 * 
	 * @return the number of bytes read.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the next byte cannot be read.
	 **/
	public int readN(byte[] buf, int off, int len)
			throws OtpErlangDecodeException {
		int i = super.read(buf, off, len);
		if (i < 0) {
			throw new OtpErlangDecodeException("Cannot read from input stream");
		}
		return i;
	}

	/**
	 * Look ahead one position in the stream without consuming the byte found
	 * there.
	 * 
	 * @return the next byte in the stream, as an integer.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the next byte cannot be read.
	 **/
	public int peek() throws OtpErlangDecodeException {
		int i;
		try {
			i = super.buf[super.pos];
			if (i < 0) {
				i += 256;
			}

			return i;
		} catch (Exception e) {
			throw new OtpErlangDecodeException("Cannot read from input stream");
		}
	}

	/**
	 * Read a one byte integer from the stream.
	 * 
	 * @return the byte read, as an integer.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the next byte cannot be read.
	 **/
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
	 * @exception OtpErlangDecodeException
	 *                if the next byte cannot be read.
	 **/
	public int read2BE() throws OtpErlangDecodeException {
		byte[] b = new byte[2];
		try {
			super.read(b);
		} catch (IOException e) {
			throw new OtpErlangDecodeException("Cannot read from input stream");
		}
		;
		return (((b[0] << 8) & 0xff00) + ((b[1]) & 0xff));
	}

	/**
	 * Read a four byte big endian integer from the stream.
	 * 
	 * @return the bytes read, converted from big endian to an integer.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the next byte cannot be read.
	 **/
	public int read4BE() throws OtpErlangDecodeException {
		byte[] b = new byte[4];
		try {
			super.read(b);
		} catch (IOException e) {
			throw new OtpErlangDecodeException("Cannot read from input stream");
		}
		;
		return (((b[0] << 24) & 0xff000000) + ((b[1] << 16) & 0xff0000)
				+ ((b[2] << 8) & 0xff00) + ((b[3]) & 0xff));
	}

	/**
	 * Read a two byte little endian integer from the stream.
	 * 
	 * @return the bytes read, converted from little endian to an integer.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the next byte cannot be read.
	 **/
	public int read2LE() throws OtpErlangDecodeException {
		byte[] b = new byte[2];
		try {
			super.read(b);
		} catch (IOException e) {
			throw new OtpErlangDecodeException("Cannot read from input stream");
		}
		;
		return (((b[1] << 8) & 0xff00) + ((b[0]) & 0xff));
	}

	/**
	 * Read a four byte little endian integer from the stream.
	 * 
	 * @return the bytes read, converted from little endian to an integer.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the next byte cannot be read.
	 **/
	public int read4LE() throws OtpErlangDecodeException {
		byte[] b = new byte[4];
		try {
			super.read(b);
		} catch (IOException e) {
			throw new OtpErlangDecodeException("Cannot read from input stream");
		}
		;
		return (((b[3] << 24) & 0xff000000) + ((b[2] << 16) & 0xff0000)
				+ ((b[1] << 8) & 0xff00) + ((b[0]) & 0xff));
	}

	/**
	 * Read a little endian integer from the stream.
	 * 
	 * @param n
	 *            the number of bytes to read
	 * 
	 * @return the bytes read, converted from little endian to an integer.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the next byte cannot be read.
	 **/
	public long readLE(int n) throws OtpErlangDecodeException {
		byte[] b = new byte[n];
		try {
			super.read(b);
		} catch (IOException e) {
			throw new OtpErlangDecodeException("Cannot read from input stream");
		}
		;
		long v = 0;
		while (n-- > 0) {
			v = (v << 8) | ((long) b[n] & 0xff);
		}
		return v;
	}

	/**
	 * Read a bigendian integer from the stream.
	 * 
	 * @param n
	 *            the number of bytes to read
	 * 
	 * @return the bytes read, converted from big endian to an integer.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the next byte cannot be read.
	 **/
	public long readBE(int n) throws OtpErlangDecodeException {
		byte[] b = new byte[n];
		try {
			super.read(b);
		} catch (IOException e) {
			throw new OtpErlangDecodeException("Cannot read from input stream");
		}
		;
		long v = 0;
		for (int i = 0; i < n; i++) {
			v = (v << 8) | ((long) b[i] & 0xff);
		}
		return v;
	}

	/**
	 * Read an Erlang atom from the stream and interpret the value as a boolean.
	 * 
	 * @return true if the atom at the current position in the stream contains
	 *         the value 'true' (ignoring case), false otherwise.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the next term in the stream is not an atom.
	 **/
	public boolean read_boolean() throws OtpErlangDecodeException {
		return Boolean.valueOf(this.read_atom()).booleanValue();
	}

	/**
	 * Read an Erlang atom from the stream.
	 * 
	 * @return a String containing the value of the atom.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the next term in the stream is not an atom.
	 **/
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
	 * @exception OtpErlangDecodeException
	 *                if the next term in the stream is not a binary.
	 **/
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
	 * Read an Erlang bitstr from the stream.
	 * 
	 * @param pad_bits
	 *            an int array whose first element will be set to the number of
	 *            pad bits in the last byte.
	 * 
	 * @return a byte array containing the value of the bitstr.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the next term in the stream is not a bitstr.
	 **/
	public byte[] read_bitstr(int pad_bits[]) throws OtpErlangDecodeException {
		int tag;
		int len;
		byte[] bin;

		tag = this.read1();
		if (tag == OtpExternal.versionTag) {
			tag = this.read1();
		}

		if (tag != OtpExternal.bitBinTag) {
			throw new OtpErlangDecodeException(
					"Wrong tag encountered, expected " + OtpExternal.bitBinTag
							+ ", got " + tag);
		}

		len = this.read4BE();
		bin = new byte[len];
		int tail_bits = this.read1();
		if (tail_bits < 0 || 7 < tail_bits) {
			throw new OtpErlangDecodeException(
					"Wrong tail bit count in bitstr: " + tail_bits);
		}
		if (len == 0 && tail_bits != 0) {
			throw new OtpErlangDecodeException(
					"Length 0 on bitstr with tail bit count: " + tail_bits);
		}
		this.readN(bin);

		pad_bits[0] = 8 - tail_bits;
		return bin;
	}

	/**
	 * Read an Erlang float from the stream.
	 * 
	 * @return the float value.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the next term in the stream is not a float.
	 **/
	public float read_float() throws OtpErlangDecodeException {
		double d = this.read_double();
		return (float) d;
	}

	/**
	 * Read an Erlang float from the stream.
	 * 
	 * @return the float value, as a double.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the next term in the stream is not a float.
	 **/
	public double read_double() throws OtpErlangDecodeException {
		int tag;

		// parse the stream
		tag = this.read1();
		if (tag == OtpExternal.versionTag) {
			tag = this.read1();
		}

		switch (tag) {
		case OtpExternal.newFloatTag: {
			return Double.longBitsToDouble(this.readBE(8));
		}
		case OtpExternal.floatTag: {
			BigDecimal val;
			int epos;
			int exp;
			byte[] strbuf = new byte[31];
			String str;

			// get the string
			this.readN(strbuf);
			str = new String(strbuf);

			// find the exponent prefix 'e' in the string
			epos = str.indexOf('e', 0);

			if (epos < 0) {
				throw new OtpErlangDecodeException("Invalid float format: '"
						+ str + "'");
			}

			// remove the sign from the exponent, if positive
			String estr = str.substring(epos + 1).trim();

			if (estr.substring(0, 1).equals("+")) {
				estr = estr.substring(1);
			}

			// now put the mantissa and exponent together
			exp = Integer.valueOf(estr).intValue();
			val = new BigDecimal(str.substring(0, epos)).movePointRight(exp);

			return val.doubleValue();
		}
		default:
			throw new OtpErlangDecodeException(
					"Wrong tag encountered, expected "
							+ OtpExternal.newFloatTag + ", got " + tag);
		}
	}

	/**
	 * Read one byte from the stream.
	 * 
	 * @return the byte read.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the next byte cannot be read.
	 **/
	public byte read_byte() throws OtpErlangDecodeException {
		long l = this.read_long(false);
		byte i = (byte) l;

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
	 * @exception OtpErlangDecodeException
	 *                if the next term in the stream is not an integer that can
	 *                be represented as a char.
	 **/
	public char read_char() throws OtpErlangDecodeException {
		long l = this.read_long(true);
		char i = (char) l;

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
	 * @exception OtpErlangDecodeException
	 *                if the next term in the stream can not be represented as a
	 *                positive integer.
	 **/
	public int read_uint() throws OtpErlangDecodeException {
		long l = this.read_long(true);
		int i = (int) l;

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
	 * @exception OtpErlangDecodeException
	 *                if the next term in the stream can not be represented as
	 *                an integer.
	 **/
	public int read_int() throws OtpErlangDecodeException {
		long l = this.read_long(false);
		int i = (int) l;

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
	 * @exception OtpErlangDecodeException
	 *                if the next term in the stream can not be represented as a
	 *                positive short.
	 **/
	public short read_ushort() throws OtpErlangDecodeException {
		long l = this.read_long(true);
		short i = (short) l;

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
	 **/
	public short read_short() throws OtpErlangDecodeException {
		long l = this.read_long(false);
		short i = (short) l;

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
	 * @exception OtpErlangDecodeException
	 *                if the next term in the stream can not be represented as a
	 *                positive long.
	 **/
	public long read_ulong() throws OtpErlangDecodeException {
		return this.read_long(true);
	}

	/**
	 * Read a long from the stream.
	 * 
	 * @return the long value.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the next term in the stream can not be represented as a
	 *                long.
	 **/
	public long read_long() throws OtpErlangDecodeException {
		return this.read_long(false);
	}

	public long read_long(boolean unsigned) throws OtpErlangDecodeException {
		byte[] b = this.read_integer_byte_array();
		return OtpInputStream.byte_array_to_long(b, unsigned);
	}

	/**
	 * Read an integer from the stream.
	 * 
	 * @return the value as a big endian 2's complement byte array.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the next term in the stream is not an integer.
	 **/
	public byte[] read_integer_byte_array() throws OtpErlangDecodeException {
		int tag;
		byte[] nb;

		tag = this.read1();
		if (tag == OtpExternal.versionTag) {
			tag = this.read1();
		}

		switch (tag) {
		case OtpExternal.smallIntTag:
			nb = new byte[2];
			nb[0] = 0;
			nb[1] = (byte) this.read1();
			break;

		case OtpExternal.intTag:
			nb = new byte[4];
			if (this.readN(nb) != 4) { // Big endian
				throw new OtpErlangDecodeException(
						"Cannot read from intput stream");
			}
			break;

		case OtpExternal.smallBigTag:
		case OtpExternal.largeBigTag:
			int arity;
			int sign;
			if (tag == OtpExternal.smallBigTag) {
				arity = this.read1();
				sign = this.read1();
			} else {
				arity = this.read4BE();
				sign = this.read1();
				if (arity + 1 < 0) {
					throw new OtpErlangDecodeException(
							"Value of largeBig does not fit in BigInteger, arity "
									+ arity + " sign " + sign);
				}
			}
			nb = new byte[arity + 1];
			// Value is read as little endian. The big end is augumented
			// with one zero byte to make the value 2's complement positive.
			if (this.readN(nb, 0, arity) != arity) {
				throw new OtpErlangDecodeException(
						"Cannot read from intput stream");
			}
			// Reverse the array to make it big endian.
			for (int i = 0, j = nb.length; i < j--; i++) {
				// Swap [i] with [j]
				byte b = nb[i];
				nb[i] = nb[j];
				nb[j] = b;
			}
			if (sign != 0) {
				// 2's complement negate the big endian value in the array
				int c = 1; // Carry
				for (int j = nb.length; j-- > 0;) {
					c = ((~nb[j]) & 0xFF) + c;
					nb[j] = (byte) c;
					c >>= 8;
				}
			}
			break;

		default:
			throw new OtpErlangDecodeException("Not valid integer tag: " + tag);
		}

		return nb;
	}

	public static long byte_array_to_long(byte[] b, boolean unsigned)
			throws OtpErlangDecodeException {
		long v;
		switch (b.length) {
		case 0:
			v = 0;
			break;
		case 2:
			v = ((b[0] & 0xFF) << 8) + (b[1] & 0xFF);
			v = (short) v; // Sign extend
			if ((v < 0) && unsigned) {
				throw new OtpErlangDecodeException("Value not unsigned: " + v);
			}
			break;
		case 4:
			v = ((b[0] & 0xFF) << 24) + ((b[0] & 0xFF) << 16)
					+ ((b[0] & 0xFF) << 8) + (b[1] & 0xFF);
			v = (int) v; // Sign extend
			if ((v < 0) && unsigned) {
				throw new OtpErlangDecodeException("Value not unsigned: " + v);
			}
		default:
			int i = 0;
			byte c = b[i];
			// Skip non-essential leading bytes
			if (unsigned) {
				if (c < 0) {
					throw new OtpErlangDecodeException("Value not unsigned: "
							+ b);
				}
				while (b[i] == 0) {
					i++; // Skip leading zero sign bytes
				}
			} else {
				if ((c == 0) || (c == -1)) { // Leading sign byte
					i = 1;
					// Skip all leading sign bytes
					while ((i < b.length) && (b[i] == c)) {
						i++;
					}
					if (i < b.length) {
						// Check first non-sign byte to see if its sign
						// matches the whole number's sign. If not one more
						// byte is needed to represent the value.
						if (((c ^ b[i]) & 0x80) != 0) {
							i--;
						}
					}
				}
			}
			if ((b.length - i) > 8) {
				// More than 64 bits of value
				throw new OtpErlangDecodeException(
						"Value does not fit in long: " + b);
			}
			// Convert the necessary bytes
			for (v = c < 0 ? -1 : 0; i < b.length; i++) {
				v = (v << 8) | (b[i] & 0xFF);
			}
		}
		return v;
	}

	/**
	 * Read a list header from the stream.
	 * 
	 * @return the arity of the list.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the next term in the stream is not a list.
	 **/
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
	 * @exception OtpErlangDecodeException
	 *                if the next term in the stream is not a tuple.
	 **/
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
	 * @exception OtpErlangDecodeException
	 *                if the next term in the stream is not an empty list.
	 **/
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
	 * @exception OtpErlangDecodeException
	 *                if the next term in the stream is not an Erlang PID.
	 **/
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
		serial = this.read4BE() & 0x1fff; // 13 bits
		creation = this.read1() & 0x03; // 2 bits

		return new OtpErlangPid(node, id, serial, creation);
	}

	/**
	 * Read an Erlang port from the stream.
	 * 
	 * @return the value of the port.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the next term in the stream is not an Erlang port.
	 **/
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
		id = this.read4BE() & 0xfffffff; // 28 bits
		creation = this.read1() & 0x03; // 2 bits

		return new OtpErlangPort(node, id, creation);
	}

	/**
	 * Read an Erlang reference from the stream.
	 * 
	 * @return the value of the reference
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the next term in the stream is not an Erlang reference.
	 **/
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
			int arity = this.read2BE();
			node = this.read_atom();
			creation = this.read1() & 0x03; // 2 bits

			int[] ids = new int[arity];
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
	 * @exception OtpErlangDecodeException
	 *                if the next term in the stream is not a string.
	 **/
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
	 * Read a compressed term from the stream
	 * 
	 * @return the resulting uncompressed term.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the next term in the stream is not a compressed term.
	 **/
	public OtpErlangObject read_compressed() throws OtpErlangDecodeException {
		int tag = this.read1();
		if (tag == OtpExternal.versionTag) {
			tag = this.read1();
		}

		if (tag != OtpExternal.compressedTag) {
			throw new OtpErlangDecodeException(
					"Wrong tag encountered, expected "
							+ OtpExternal.compressedTag + ", got " + tag);
		}

		int size = this.read4BE();
		byte[] buf = new byte[size];
		java.util.zip.InflaterInputStream is = new java.util.zip.InflaterInputStream(
				this);
		try {
			int dsize = is.read(buf, 0, size);
			if (dsize != size) {
				throw new OtpErlangDecodeException("Decompression gave "
						+ dsize + " bytes, not " + size);
			}
		} catch (IOException e) {
			throw new OtpErlangDecodeException("Cannot read from input stream");
		}

		OtpInputStream ois = new OtpInputStream(buf);
		return ois.read_any();
	}

	/**
	 * Read an arbitrary Erlang term from the stream.
	 * 
	 * @return the Erlang term.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the stream does not contain a known Erlang type at the
	 *                next position.
	 **/
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
		case OtpExternal.smallBigTag:
		case OtpExternal.largeBigTag:
			return new OtpErlangLong(this);

		case OtpExternal.atomTag:
			return new OtpErlangAtom(this);

		case OtpExternal.floatTag:
		case OtpExternal.newFloatTag:
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

		case OtpExternal.bitBinTag:
			return new OtpErlangBitstr(this);

		case OtpExternal.compressedTag:
			return read_compressed();

		default:
			throw new OtpErlangDecodeException("Uknown data type: " + tag);
		}
	}
}
