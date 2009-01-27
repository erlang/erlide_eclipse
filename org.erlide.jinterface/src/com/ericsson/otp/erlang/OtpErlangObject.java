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
 * Base class of the Erlang data type classes. This class is used to represent
 * an arbitrary Erlang term.
 **/
public abstract class OtpErlangObject implements Serializable, Cloneable {
	// don't change this!
	static final long serialVersionUID = -8435938572339430044L;

	/**
	 * @return the printable representation of the object. This is usually
	 *         similar to the representation used by Erlang for the same type of
	 *         object.
	 **/
	@Override
	public abstract String toString();

	/**
	 * Convert the object according to the rules of the Erlang external format.
	 * This is mainly used for sending Erlang terms in messages, however it can
	 * also be used for storing terms to disk.
	 * 
	 * @param buf
	 *            an output stream to which the encoded term should be written.
	 **/
	public abstract void encode(OtpOutputStream buf);

	/**
	 * Read binary data in the Erlang external format, and produce a
	 * corresponding Erlang data type object. This method is normally used when
	 * Erlang terms are received in messages, however it can also be used for
	 * reading terms from disk.
	 * 
	 * @param buf
	 *            an input stream containing one or more encoded Erlang terms.
	 * 
	 * @return an object representing one of the Erlang data types.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the stream does not contain a valid representation of
	 *                an Erlang term.
	 **/
	public static OtpErlangObject decode(OtpInputStream buf)
			throws OtpErlangDecodeException {
		return buf.read_any();
	}

	/**
	 * Determine if two Erlang objects are equal. In general, Erlang objects are
	 * equal if the components they consist of are equal.
	 * 
	 * @param o
	 *            the object to compare to.
	 * 
	 * @return true if the objects are identical.
	 **/
	@Override
	public abstract boolean equals(Object o);

	@Override
	public Object clone() {
		try {
			return super.clone();
		} catch (CloneNotSupportedException e) {
			/* cannot happen */
			throw new InternalError(e.toString());
		}
	}
}
