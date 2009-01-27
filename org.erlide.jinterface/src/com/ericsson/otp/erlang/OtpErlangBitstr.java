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
 * Portions created by Ericsson are Copyright 2007, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 *
 *     $Id$
 */
package com.ericsson.otp.erlang;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.Serializable;

/**
 * Provides a Java representation of Erlang bitstrs. An Erlang bitstr is an
 * Erlang binary with a length not an integral number of bytes (8-bit). Anything
 * can be represented as a sequence of bytes can be made into an Erlang bitstr.
 **/
public class OtpErlangBitstr extends OtpErlangObject implements Serializable,
		Cloneable {
	// don't change this!
	static final long serialVersionUID = -3781009633593609217L;

	protected byte[] bin;
	protected int pad_bits;

	/**
	 * Create a bitstr from a byte array
	 * 
	 * @param bin
	 *            the array of bytes from which to create the bitstr.
	 **/
	public OtpErlangBitstr(byte[] bin) {
		this.bin = new byte[bin.length];
		System.arraycopy(bin, 0, this.bin, 0, bin.length);
		this.pad_bits = 0;
	}

	/**
	 * Create a bitstr with pad bits from a byte array.
	 * 
	 * @param bin
	 *            the array of bytes from which to create the bitstr.
	 * @param pad_bits
	 *            the number of unused bits in the low end of the last byte.
	 **/
	public OtpErlangBitstr(byte[] bin, int pad_bits) {
		this.bin = new byte[bin.length];
		System.arraycopy(bin, 0, this.bin, 0, bin.length);
		this.pad_bits = pad_bits;

		check_bitstr(this.bin, this.pad_bits);
	}

	private void check_bitstr(byte[] bin, int pad_bits) {
		if (pad_bits < 0 || 7 < pad_bits) {
			throw new java.lang.IllegalArgumentException(
					"Padding must be in range 0..7");
		}
		if (pad_bits != 0 && bin.length == 0) {
			throw new java.lang.IllegalArgumentException(
					"Padding on zero length bitstr");
		}
		if (bin.length != 0) {
			// Make sure padding is zero
			bin[bin.length - 1] &= ~((1 << pad_bits) - 1);
		}
	}

	/**
	 * Create a bitstr from a stream containing a bitstr encoded in Erlang
	 * external format.
	 * 
	 * @param buf
	 *            the stream containing the encoded bitstr.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the buffer does not contain a valid external
	 *                representation of an Erlang bitstr.
	 **/
	public OtpErlangBitstr(OtpInputStream buf) throws OtpErlangDecodeException {
		int pbs[] = { 0 }; // This is ugly just to get a value-result parameter
		this.bin = buf.read_bitstr(pbs);
		this.pad_bits = pbs[0];

		check_bitstr(this.bin, this.pad_bits);
	}

	/**
	 * Create a bitstr from an arbitrary Java Object. The object must implement
	 * java.io.Serializable or java.io.Externalizable.
	 * 
	 * @param o
	 *            the object to serialize and create this bitstr from.
	 **/
	public OtpErlangBitstr(Object o) {
		try {
			this.bin = toByteArray(o);
			this.pad_bits = 0;
		} catch (IOException e) {
			throw new java.lang.IllegalArgumentException(
					"Object must implement Serializable");
		}
	}

	private static byte[] toByteArray(Object o) throws java.io.IOException {

		if (o == null) {
			return null;
		}

		/* need to synchronize use of the shared baos */
		java.io.ByteArrayOutputStream baos = new ByteArrayOutputStream();
		java.io.ObjectOutputStream oos = new java.io.ObjectOutputStream(baos);

		oos.writeObject(o);
		oos.flush();

		return baos.toByteArray();
	}

	private static Object fromByteArray(byte[] buf) {
		if (buf == null) {
			return null;
		}

		try {
			java.io.ByteArrayInputStream bais = new java.io.ByteArrayInputStream(
					buf);
			java.io.ObjectInputStream ois = new java.io.ObjectInputStream(bais);
			return ois.readObject();
		} catch (java.lang.ClassNotFoundException e) {
		} catch (java.io.IOException e) {
		}

		return null;
	}

	/**
	 * Get the byte array from a bitstr, padded with zero bits in the little end
	 * of the last byte.
	 * 
	 * @return the byte array containing the bytes for this bitstr.
	 **/
	public byte[] binaryValue() {
		return this.bin;
	}

	/**
	 * Get the size in whole bytes of the bitstr, rest bits in the last byte not
	 * counted.
	 * 
	 * @return the number of bytes contained in the bintstr.
	 **/
	public int size() {
		if (this.pad_bits == 0) {
			return this.bin.length;
		}
		if (this.bin.length == 0) {
			throw new java.lang.IllegalStateException("Impossible length");
		}
		return this.bin.length - 1;
	}

	/**
	 * Get the number of pad bits in the last byte of the bitstr. The pad bits
	 * are zero and in the little end.
	 * 
	 * @return the number of pad bits in the bitstr.
	 **/
	public int pad_bits() {
		return this.pad_bits;
	}

	/**
	 * Get the java Object from the bitstr. If the bitstr contains a serialized
	 * Java object, then this method will recreate the object.
	 * 
	 * 
	 * @return the java Object represented by this bitstr, or null if the bitstr
	 *         does not represent a Java Object.
	 **/
	public Object getObject() {
		if (this.pad_bits != 0) {
			return null;
		}
		return fromByteArray(this.bin);
	}

	/**
	 * Get the string representation of this bitstr object. A bitstr is printed
	 * as #Bin&lt;N&gt;, where N is the number of bytes contained in the object
	 * or #bin&lt;N-M&gt; if there are M pad bits.
	 * 
	 * @return the Erlang string representation of this bitstr.
	 **/
	@Override
	public String toString() {
		if (this.pad_bits == 0) {
			return "#Bin<" + this.bin.length + ">";
		}
		if (this.bin.length == 0) {
			throw new java.lang.IllegalStateException("Impossible length");
		}
		return "#Bin<" + this.bin.length + "-" + this.pad_bits + ">";
	}

	/**
	 * Convert this bitstr to the equivalent Erlang external representation.
	 * 
	 * @param buf
	 *            an output stream to which the encoded bitstr should be
	 *            written.
	 **/
	@Override
	public void encode(OtpOutputStream buf) {
		buf.write_bitstr(this.bin, this.pad_bits);
	}

	/**
	 * Determine if two bitstrs are equal. Bitstrs are equal if they have the
	 * same byte length and tail length, and the array of bytes is identical.
	 * 
	 * @param o
	 *            the bitstr to compare to.
	 * 
	 * @return true if the bitstrs contain the same bits, false otherwise.
	 **/
	@Override
	public boolean equals(Object o) {
		if (!(o instanceof OtpErlangBitstr)) {
			return false;
		}

		OtpErlangBitstr that = (OtpErlangBitstr) o;
		if (this.pad_bits != that.pad_bits) {
			return false;
		}

		int len = this.bin.length;
		if (len != that.bin.length) {
			return false;
		}

		for (int i = 0; i < len; i++) {
			if (this.bin[i] != that.bin[i]) {
				return false; // early exit
			}
		}

		return true;
	}

	@Override
	public Object clone() {
		OtpErlangBitstr that = (OtpErlangBitstr) (super.clone());
		that.bin = this.bin.clone();
		that.pad_bits = this.pad_bits;
		return that;
	}
}
