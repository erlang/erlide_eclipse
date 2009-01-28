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

//import java.io.OutputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.DecimalFormat;

/**
 * Provides a stream for encoding Erlang terms to external format, for
 * transmission or storage.
 * 
 * <p>
 * Note that this class is not synchronized, if you need synchronization you
 * must provide it yourself.
 * 
 **/
public class OtpOutputStream extends ByteArrayOutputStream {
	/** The default initial size of the stream. **/
	public static final int defaultInitialSize = 2048;

	/** The default increment used when growing the stream. **/
	public static final int defaultIncrement = 2048;

	// static formats, used to encode floats and doubles
	private static final DecimalFormat eform = new DecimalFormat("e+00;e-00");
	private static final BigDecimal ten = new BigDecimal(10.0);
	private static final BigDecimal one = new BigDecimal(1.0);

	/**
	 * Create a stream with the default initial size (2048 bytes).
	 **/
	public OtpOutputStream() {
		this(defaultInitialSize);
	}

	/**
	 * Create a stream with the specified initial size.
	 **/
	public OtpOutputStream(int size) {
		super(size);
	}

	/**
	 * Create a stream containing the encoded version of the given Erlang term.
	 **/
	public OtpOutputStream(OtpErlangObject o) {
		this();
		this.write_any(o);
	}

	// package scope
	/*
	 * Get the contents of the output stream as an input stream instead. This is
	 * used internally in {@link OtpCconnection} for tracing outgoing packages.
	 * 
	 * @param offset where in the output stream to read data from when creating
	 * the input stream. The offset is necessary because header contents start 5
	 * bytes into the header buffer, whereas payload contents start at the
	 * beginning
	 * 
	 * @return an input stream containing the same raw data.
	 */
	OtpInputStream getOtpInputStream(int offset) {
		return new OtpInputStream(super.buf, offset, super.count - offset);
	}

	/**
	 * Get the current position in the stream.
	 * 
	 * @return the current position in the stream.
	 **/
	public int getPos() {
		return super.count;
	}

	/**
	 * Write one byte to the stream.
	 * 
	 * @param b
	 *            the byte to write.
	 * 
	 **/
	public void write(byte b) {
		if (super.count >= super.buf.length) {
			// System.err.println("Expanding buffer from " + this.buf.length
			// + " to " + (this.buf.length+defaultIncrement));
			byte[] tmp = new byte[super.buf.length + defaultIncrement];
			System.arraycopy(super.buf, 0, tmp, 0, super.count);
			super.buf = tmp;
		}
		super.buf[super.count++] = b;
	}

	/**
	 * Write an array of bytes to the stream.
	 * 
	 * @param buf
	 *            the array of bytes to write.
	 * 
	 **/

	public void write(byte[] buf) {
		if (super.count + buf.length > super.buf.length) {
			// System.err.println("Expanding buffer from " + super.buf.length
			// + " to " + (buf.length + super.buf.lengt + defaultIncrement));
			byte[] tmp = new byte[super.buf.length + buf.length
					+ defaultIncrement];
			System.arraycopy(super.buf, 0, tmp, 0, super.count);
			super.buf = tmp;
		}
		System.arraycopy(buf, 0, super.buf, super.count, buf.length);
		super.count += buf.length;
	}

	/**
	 * Write the low byte of a value to the stream.
	 * 
	 * @param n
	 *            the value to use.
	 * 
	 **/
	public void write1(long n) {
		write((byte) (n & 0xff));
	}

	/**
	 * Write an array of bytes to the stream.
	 * 
	 * @param buf
	 *            the array of bytes to write.
	 * 
	 **/
	public void writeN(byte[] bytes) {
		write(bytes);
	}

	/**
	 * Get the current capacity of the stream. As bytes are added the capacity
	 * of the stream is increased automatically, however this method returns the
	 * current size.
	 * 
	 * @return the size of the internal buffer used by the stream.
	 **/
	public int length() {
		return super.buf.length;
	}

	/**
	 * Get the number of bytes in the stream.
	 * 
	 * @return the number of bytes in the stream.
	 * 
	 * @deprecated As of Jinterface 1.4, replaced by super.size().
	 * @see #size()
	 **/

	public int count() {
		return count;
	}

	/**
	 * Write the low two bytes of a value to the stream in big endian order.
	 * 
	 * @param n
	 *            the value to use.
	 **/
	public void write2BE(long n) {
		write((byte) ((n & 0xff00) >> 8));
		write((byte) (n & 0xff));
	}

	/**
	 * Write the low four bytes of a value to the stream in big endian order.
	 * 
	 * @param n
	 *            the value to use.
	 **/
	public void write4BE(long n) {
		write((byte) ((n & 0xff000000) >> 24));
		write((byte) ((n & 0xff0000) >> 16));
		write((byte) ((n & 0xff00) >> 8));
		write((byte) (n & 0xff));
	}

	/**
	 * Write the low eight (all) bytes of a value to the stream in big endian
	 * order.
	 * 
	 * @param n
	 *            the value to use.
	 **/
	public void write8BE(long n) {
		write((byte) ((n >> 56) & 0xff));
		write((byte) ((n >> 48) & 0xff));
		write((byte) ((n >> 40) & 0xff));
		write((byte) ((n >> 32) & 0xff));
		write((byte) ((n >> 24) & 0xff));
		write((byte) ((n >> 16) & 0xff));
		write((byte) ((n >> 8) & 0xff));
		write((byte) (n & 0xff));
	}

	/**
	 * Write any number of bytes in little endian format.
	 * 
	 * @param n
	 *            the value to use.
	 * @param b
	 *            the number of bytes to write from the little end.
	 **/
	public void writeLE(long n, int b) {
		for (int i = 0; i < b; i++) {
			write((byte) (n & 0xff));
			n >>= 8;
		}
	}

	/**
	 * Write the low two bytes of a value to the stream in little endian order.
	 * 
	 * @param n
	 *            the value to use.
	 **/
	public void write2LE(long n) {
		write((byte) (n & 0xff));
		write((byte) ((n & 0xff00) >> 8));
	}

	/**
	 * Write the low four bytes of a value to the stream in little endian order.
	 * 
	 * @param n
	 *            the value to use.
	 **/
	public void write4LE(long n) {
		write((byte) (n & 0xff));
		write((byte) ((n & 0xff00) >> 8));
		write((byte) ((n & 0xff0000) >> 16));
		write((byte) ((n & 0xff000000) >> 24));
	}

	/**
	 * Write the low eight bytes of a value to the stream in little endian
	 * order.
	 * 
	 * @param n
	 *            the value to use.
	 **/
	public void write8LE(long n) {
		write((byte) ((n) & 0xff));
		write((byte) ((n >> 8) & 0xff));
		write((byte) ((n >> 16) & 0xff));
		write((byte) ((n >> 24) & 0xff));
		write((byte) ((n >> 32) & 0xff));
		write((byte) ((n >> 40) & 0xff));
		write((byte) ((n >> 48) & 0xff));
		write((byte) ((n >> 56) & 0xff));
	}

	/**
	 * Write the low four bytes of a value to the stream in bif endian order, at
	 * the specified position. If the position specified is beyond the end of
	 * the stream, this method will have no effect.
	 * 
	 * Normally this method should be used in conjunction with {@link #size()
	 * size()}, when is is necessary to insert data into the stream before it is
	 * known what the actual value should be. For example:
	 * 
	 * <pre>
	 * int pos = s.size();
	 *    s.write4BE(0); // make space for length data,
	 *                   // but final value is not yet known
	 *     [ ...more write statements...]
	 *    // later... when we know the length value
	 *    s.poke4BE(pos, length);
	 * </pre>
	 * 
	 * 
	 * @param offset
	 *            the position in the stream.
	 * @param n
	 *            the value to use.
	 **/
	public void poke4BE(int offset, long n) {
		if (offset < super.count) {
			buf[offset + 0] = ((byte) ((n & 0xff000000) >> 24));
			buf[offset + 1] = ((byte) ((n & 0xff0000) >> 16));
			buf[offset + 2] = ((byte) ((n & 0xff00) >> 8));
			buf[offset + 3] = ((byte) (n & 0xff));
		}
	}

	/**
	 * Write a string to the stream as an Erlang atom.
	 * 
	 * @param atom
	 *            the string to write.
	 **/
	public void write_atom(String atom) {
		this.write1(OtpExternal.atomTag);
		this.write2BE(atom.length());
		this.writeN(atom.getBytes());
	}

	/**
	 * Write an array of bytes to the stream as an Erlang binary.
	 * 
	 * @param bin
	 *            the array of bytes to write.
	 **/
	public void write_binary(byte[] bin) {
		this.write1(OtpExternal.binTag);
		this.write4BE(bin.length);
		this.writeN(bin);
	}

	/**
	 * Write an array of bytes to the stream as an Erlang bitstr.
	 * 
	 * @param bin
	 *            the array of bytes to write.
	 * @param pad_bits
	 *            the number of zero pad bits at the low end of the last byte
	 **/
	public void write_bitstr(byte[] bin, int pad_bits) {
		if (pad_bits == 0) {
			write_binary(bin);
			return;
		}
		this.write1(OtpExternal.bitBinTag);
		this.write4BE(bin.length);
		this.write1(8 - pad_bits);
		this.writeN(bin);
	}

	/**
	 * Write a boolean value to the stream as the Erlang atom 'true' or 'false'.
	 * 
	 * @param b
	 *            the boolean value to write.
	 **/
	public void write_boolean(boolean b) {
		this.write_atom(String.valueOf(b));
	}

	/**
	 * Write a single byte to the stream as an Erlang integer. The byte is
	 * really an IDL 'octet', that is, unsigned.
	 * 
	 * @param b
	 *            the byte to use.
	 **/
	public void write_byte(byte b) {
		this.write_long(b & 0xffL, true);
	}

	/**
	 * Write a character to the stream as an Erlang integer. The character may
	 * be a 16 bit character, kind of IDL 'wchar'. It is up to the Erlang side
	 * to take care of souch, if they should be used.
	 * 
	 * @param c
	 *            the character to use.
	 **/
	public void write_char(char c) {
		this.write_long(c & 0xffffL, true);
	}

	/**
	 * Write a double value to the stream.
	 * 
	 * @param d
	 *            the double to use.
	 **/
	public void write_double(double d) {
		this.write1(OtpExternal.newFloatTag);
		this.write8BE(Double.doubleToLongBits(d));
	}

	/**
	 * Write a float value to the stream.
	 * 
	 * @param f
	 *            the float to use.
	 **/
	public void write_float(float f) {
		this.write_double(f);
	}

	public void write_big_integer(BigInteger v) {
		if (v.bitLength() < 64) {
			this.write_long(v.longValue(), true);
			return;
		}
		int signum = v.signum();
		if (signum < 0) {
			v = v.negate();
		}
		byte[] magnitude = v.toByteArray();
		int n = magnitude.length;
		// Reverse the array to make it little endian.
		for (int i = 0, j = n; i < j--; i++) {
			// Swap [i] with [j]
			byte b = magnitude[i];
			magnitude[i] = magnitude[j];
			magnitude[j] = b;
		}
		if ((n & 0xFF) == n) {
			this.write1(OtpExternal.smallBigTag);
			this.write1(n); // length
		} else {
			this.write1(OtpExternal.largeBigTag);
			this.write4BE(n); // length
		}
		this.write1(signum < 0 ? 1 : 0); // sign
		// Write the array
		this.writeN(magnitude);
	}

	void write_long(long v, boolean unsigned) {
		/*
		 * If v<0 and unsigned==true the value
		 * java.lang.Long.MAX_VALUE-java.lang.Long.MIN_VALUE+1+v is written, i.e
		 * v is regarded as unsigned two's complement.
		 */
		if ((v & 0xffL) == v) {
			// will fit in one byte
			this.write1(OtpExternal.smallIntTag);
			this.write1(v);
		} else {
			// note that v != 0L
			if ((v < 0 && unsigned) || v < OtpExternal.erlMin
					|| v > OtpExternal.erlMax) {
				// some kind of bignum
				long abs = (unsigned ? v : (v < 0 ? -v : v));
				int sign = (unsigned ? 0 : (v < 0 ? 1 : 0));
				int n;
				long mask;
				for (mask = 0xFFFFffffL, n = 4; (abs & mask) != abs; n++, mask = (mask << 8) | 0xffL) {
					; // count nonzero bytes
				}
				this.write1(OtpExternal.smallBigTag);
				this.write1(n); // length
				this.write1(sign); // sign
				this.writeLE(abs, n); // value. obs! little endian
			} else {
				this.write1(OtpExternal.intTag);
				this.write4BE(v);
			}
		}
	}

	/**
	 * Write a long to the stream.
	 * 
	 * @param l
	 *            the long to use.
	 **/
	public void write_long(long l) {
		this.write_long(l, false);
	}

	/**
	 * Write a positive long to the stream. The long is interpreted as a two's
	 * complement unsigned long even if it is negative.
	 * 
	 * @param ul
	 *            the long to use.
	 **/
	public void write_ulong(long ul) {
		this.write_long(ul, true);
	}

	/**
	 * Write an integer to the stream.
	 * 
	 * @param i
	 *            the integer to use.
	 **/
	public void write_int(int i) {
		this.write_long(i, false);
	}

	/**
	 * Write a positive integer to the stream. The integer is interpreted as a
	 * two's complement unsigned integer even if it is negative.
	 * 
	 * @param ui
	 *            the integer to use.
	 **/
	public void write_uint(int ui) {
		this.write_long(ui & 0xFFFFffffL, true);
	}

	/**
	 * Write a short to the stream.
	 * 
	 * @param s
	 *            the short to use.
	 **/
	public void write_short(short s) {
		this.write_long(s, false);
	}

	/**
	 * Write a positive short to the stream. The short is interpreted as a two's
	 * complement unsigned short even if it is negative.
	 * 
	 * @param s
	 *            the short to use.
	 **/
	public void write_ushort(short us) {
		this.write_long(us & 0xffffL, true);
	}

	/**
	 * Write an Erlang list header to the stream. After calling this method, you
	 * must write 'arity' elements to the stream followed by nil, or it will not
	 * be possible to decode it later.
	 * 
	 * @param arity
	 *            the number of elements in the list.
	 **/
	public void write_list_head(int arity) {
		if (arity == 0) {
			this.write_nil();
		} else {
			this.write1(OtpExternal.listTag);
			this.write4BE(arity);
		}
	}

	/**
	 * Write an empty Erlang list to the stream.
	 **/
	public void write_nil() {
		this.write1(OtpExternal.nilTag);
	}

	/**
	 * Write an Erlang tuple header to the stream. After calling this method,
	 * you must write 'arity' elements to the stream or it will not be possible
	 * to decode it later.
	 * 
	 * @param arity
	 *            the number of elements in the tuple.
	 **/
	public void write_tuple_head(int arity) {
		if (arity < 0xff) {
			this.write1(OtpExternal.smallTupleTag);
			this.write1(arity);
		} else {
			this.write1(OtpExternal.largeTupleTag);
			this.write4BE(arity);
		}
	}

	/**
	 * Write an Erlang PID to the stream.
	 * 
	 * @param node
	 *            the nodename.
	 * 
	 * @param id
	 *            an arbitrary number. Only the low order 15 bits will be used.
	 * 
	 * @param serial
	 *            another arbitrary number. Only the low order 13 bits will be
	 *            used.
	 * 
	 * @param creation
	 *            yet another arbitrary number. Only the low order 2 bits will
	 *            be used.
	 * 
	 **/
	public void write_pid(String node, int id, int serial, int creation) {
		this.write1(OtpExternal.pidTag);
		this.write_atom(node);
		this.write4BE(id & 0x7fff); // 15 bits
		this.write4BE(serial & 0x1fff); // 13 bits
		this.write1(creation & 0x3); // 2 bits
	}

	/**
	 * Write an Erlang port to the stream.
	 * 
	 * @param node
	 *            the nodename.
	 * 
	 * @param id
	 *            an arbitrary number. Only the low order 28 bits will be used.
	 * 
	 * @param creation
	 *            another arbitrary number. Only the low order 2 bits will be
	 *            used.
	 * 
	 **/
	public void write_port(String node, int id, int creation) {
		this.write1(OtpExternal.portTag);
		this.write_atom(node);
		this.write4BE(id & 0xfffffff); // 28 bits
		this.write1(creation & 0x3); // 2 bits
	}

	/**
	 * Write an old style Erlang ref to the stream.
	 * 
	 * @param node
	 *            the nodename.
	 * 
	 * @param id
	 *            an arbitrary number. Only the low order 18 bits will be used.
	 * 
	 * @param creation
	 *            another arbitrary number. Only the low order 2 bits will be
	 *            used.
	 * 
	 **/
	public void write_ref(String node, int id, int creation) {
		this.write1(OtpExternal.refTag);
		this.write_atom(node);
		this.write4BE(id & 0x3ffff); // 18 bits
		this.write1(creation & 0x3); // 2 bits
	}

	/**
	 * Write a new style (R6 and later) Erlang ref to the stream.
	 * 
	 * @param node
	 *            the nodename.
	 * 
	 * @param ids
	 *            an array of arbitrary numbers. Only the low order 18 bits of
	 *            the first number will be used. If the array contains only one
	 *            number, an old style ref will be written instead. At most
	 *            three numbers will be read from the array.
	 * 
	 * @param creation
	 *            another arbitrary number. Only the low order 2 bits will be
	 *            used.
	 * 
	 **/
	public void write_ref(String node, int[] ids, int creation) {
		int arity = ids.length;
		if (arity > 3) {
			arity = 3; // max 3 words in ref
		}

		if (arity == 1) {
			// use old method
			this.write_ref(node, ids[0], creation);
		} else {
			// r6 ref
			this.write1(OtpExternal.newRefTag);

			// how many id values
			this.write2BE(arity);

			this.write_atom(node);

			// note: creation BEFORE id in r6 ref
			this.write1(creation & 0x3); // 2 bits

			// first int gets truncated to 18 bits
			this.write4BE(ids[0] & 0x3ffff);

			// remaining ones are left as is
			for (int i = 1; i < arity; i++) {
				this.write4BE(ids[i]);
			}
		}
	}

	/**
	 * Write a string to the stream.
	 * 
	 * @param s
	 *            the string to write.
	 **/
	public void write_string(String s) {
		int len = s.length();

		switch (len) {
		case 0:
			this.write_nil();
			break;
		default:
			byte[] bytebuf = s.getBytes();

			/*
			 * switch to se if the length of the byte array is equal to the
			 * length of the list, or if the string is too long for the stream
			 * protocol
			 */
			if ((bytebuf.length == len) && (len <= 65535)) { /* Usual */
				this.write1(OtpExternal.stringTag);
				this.write2BE(len);
				this.writeN(bytebuf);
			} else { /* Unicode */
				char[] charbuf = s.toCharArray();

				this.write_list_head(len);

				for (int i = 0; i < len; i++) {
					this.write_char(charbuf[i]);
				}

				this.write_nil();
			}
		}
	}

	/*
	 * This does not work when char > 1 byte Unicode is used
	 * 
	 * public void write_string(String s) { this.write1(OtpExternal.stringTag);
	 * this.write2BE(s.length()); this.writeN(s.getBytes()); }
	 */
	/**
	 * Write an arbitrary Erlang term to the stream in compressed format.
	 * 
	 * @param o
	 *            the Erlang tem to write.
	 */
	public void write_compressed(OtpErlangObject o) {
		OtpOutputStream oos = new OtpOutputStream(o);
		this.write1(OtpExternal.compressedTag);
		this.write4BE(oos.size());
		java.io.FilterOutputStream fos = new java.io.FilterOutputStream(this);
		java.util.zip.DeflaterOutputStream dos = new java.util.zip.DeflaterOutputStream(
				fos);
		try {
			oos.writeTo(dos);
			dos.close();
		} catch (IOException e) {
			throw new java.lang.IllegalArgumentException(
					"Intremediate stream failed for Erlang object " + o);
		}
	}

	/**
	 * Write an arbitrary Erlang term to the stream.
	 * 
	 * @param o
	 *            the Erlang term to write.
	 */
	public void write_any(OtpErlangObject o) {
		// calls one of the above functions, depending on o
		o.encode(this);
	}
}
