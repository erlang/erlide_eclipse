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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.Serializable;

/**
 * Provides a Java representation of Erlang binaries. Anything that can be
 * represented as a sequence of bytes can be made into an Erlang binary.
 */
public class OtpErlangBinary extends OtpErlangObject implements Serializable,
		Cloneable {

	// don't change this!
	static final long serialVersionUID = -3781009633593609217L;

	// binary contents
	private byte[] bin;

	/**
	 * Create a binary from a byte array
	 * 
	 * @param bin
	 *            the array of bytes from which to create the binary.
	 */
	public OtpErlangBinary(byte[] bin) {
		this.bin = new byte[bin.length];
		System.arraycopy(bin, 0, this.bin, 0, bin.length);
	}

	/**
	 * Create a binary from a stream containinf a binary encoded in Erlang
	 * external format.
	 * 
	 * @param buf
	 *            the stream containing the encoded binary.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the buffer does not contain a valid external
	 *                representation of an Erlang binary.
	 */
	public OtpErlangBinary(OtpInputStream buf) throws OtpErlangDecodeException {
		bin = buf.read_binary();
	}

	/**
	 * Create a binary from an arbitrary Java Object. The object must implement
	 * java.io.Serializable or java.io.Externalizable.
	 * 
	 * @param o
	 *            the object to serialize and create this binary from.
	 */
	public OtpErlangBinary(Object o) {
		try {
			bin = toByteArray(o);
		} catch (final IOException e) {
			throw new java.lang.IllegalArgumentException(
					"Object must implement Serializable");
		}
	}

	private static byte[] toByteArray(Object o) throws java.io.IOException {

		if (o == null) {
			return null;
		}

		/* need to synchronize use of the shared baos */
		final java.io.ByteArrayOutputStream baos = new ByteArrayOutputStream();
		final java.io.ObjectOutputStream oos = new java.io.ObjectOutputStream(
				baos);

		oos.writeObject(o);
		oos.flush();

		return baos.toByteArray();
	}

	private static Object fromByteArray(byte[] buf) {
		if (buf == null) {
			return null;
		}

		try {
			final java.io.ByteArrayInputStream bais = new java.io.ByteArrayInputStream(
					buf);
			final java.io.ObjectInputStream ois = new java.io.ObjectInputStream(
					bais);
			return ois.readObject();
		} catch (final java.lang.ClassNotFoundException e) {
		} catch (final java.io.IOException e) {
		}

		return null;
	}

	/**
	 * Get the byte array from a binary.
	 * 
	 * @return the byte array containing the bytes for this binary.
	 */
	public byte[] binaryValue() {
		return bin;
	}

	/**
	 * Get the size of the binary.
	 * 
	 * @return the number of bytes contained in the binary.
	 */
	public int size() {
		return bin.length;
	}

	/**
	 * Get the java Object from the binary. If the binary contains a serialized
	 * Java object, then this method will recreate the object.
	 * 
	 * 
	 * @return the java Object represented by this binary, or null if the binary
	 *         does not represent a Java Object.
	 */
	public Object getObject() {
		return fromByteArray(bin);
	}

	/**
	 * Get the string representation of this binary object. A binary is printed
	 * as #Bin&lt;N&gt;, where N is the number of bytes contained in the object.
	 * 
	 * @return the Erlang string representation of this binary.
	 */
	@Override
	public String toString() {
		return "#Bin<" + bin.length + ">";
	}

	/**
	 * Convert this binary to the equivalent Erlang external representation.
	 * 
	 * @param buf
	 *            an output stream to which the encoded binary should be
	 *            written.
	 */
	@Override
	public void encode(OtpOutputStream buf) {
		buf.write_binary(bin);
	}

	/**
	 * Determine if two binaries are equal. Binaries are equal if they have the
	 * same length and the array of bytes is identical.
	 * 
	 * @param o
	 *            the binary to compare to.
	 * 
	 * @return true if the byte arrays contain the same bytes, false otherwise.
	 */
	@Override
	public boolean equals(Object o) {
		if (!(o instanceof OtpErlangBinary)) {
			return false;
		}

		final OtpErlangBinary bin_ = (OtpErlangBinary) o;
		final int size = this.size();

		if (size != bin_.size()) {
			return false;
		}

		for (int i = 0; i < size; i++) {
			if (bin[i] != bin_.bin[i]) {
				return false; // early exit
			}
		}

		return true;
	}

	@Override
	public int hashCode() {
		return new String(bin).hashCode();
	}

	@Override
	public Object clone() {
		final OtpErlangBinary newBin = (OtpErlangBinary) (super.clone());
		newBin.bin = bin.clone();
		return newBin;
	}
}
