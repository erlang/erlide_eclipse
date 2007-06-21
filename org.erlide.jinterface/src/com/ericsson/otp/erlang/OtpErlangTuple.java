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
 * Provides a Java representation of Erlang tuples. Tuples are created from one
 * or more arbitrary Erlang terms.
 * 
 * <p>
 * The arity of the tuple is the number of elements it contains. Elements are
 * indexed from 0 to (arity-1) and can be retrieved individually by using the
 * appropriate index.
 */
public class OtpErlangTuple extends OtpErlangObject implements Serializable,
		Cloneable {

	// don't change this!
	static final long serialVersionUID = 9163498658004915935L;

	private OtpErlangObject[] elems = null;

	/**
	 * Create a unary tuple containing the given element.
	 * 
	 * @param elem
	 *            the element to create the tuple from.
	 * 
	 * @exception java.lang.IllegalArgumentException
	 *                if the array is empty (null).
	 */
	public OtpErlangTuple(OtpErlangObject elem) {
		if (elem == null) {
			throw new java.lang.IllegalArgumentException(
					"Tuple element cannot be null");
		}
		elems = new OtpErlangObject[1];
		elems[0] = elem;
	}

	/**
	 * Create a tuple from an array of terms.
	 * 
	 * @param elems
	 *            the array of terms to create the tuple from.
	 * 
	 * @exception java.lang.IllegalArgumentException
	 *                if the array is empty (null) or contains null elements.
	 */
	public OtpErlangTuple(OtpErlangObject[] elems) {
		this(elems, 0, elems.length);
	}

	/**
	 * Create a tuple from an array of terms.
	 * 
	 * @param elems
	 *            the array of terms to create the tuple from.
	 * @param start
	 *            the offset of the first term to insert.
	 * @param count
	 *            the number of terms to insert.
	 * 
	 * @exception java.lang.IllegalArgumentException
	 *                if the array is empty (null) or contains null elements.
	 */
	public OtpErlangTuple(OtpErlangObject[] elems, int start, int count) {
		if (elems == null) {
			throw new java.lang.IllegalArgumentException(
					"Cannot make an empty tuple");
		} else {
			this.elems = new OtpErlangObject[count];
			for (int i = 0; i < count; i++) {
				if (elems[start + i] != null) {
					this.elems[i] = elems[start + i];
				} else {
					throw new java.lang.IllegalArgumentException(
							"Tuple element cannot be null (element" +
									(start + i) + ")");
				}
			}
		}
	}

	/**
	 * Create a tuple from a stream containing an tuple encoded in Erlang
	 * external format.
	 * 
	 * @param buf
	 *            the stream containing the encoded tuple.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the buffer does not contain a valid external
	 *                representation of an Erlang tuple.
	 */
	public OtpErlangTuple(OtpInputStream buf) throws OtpErlangDecodeException {
		final int arity = buf.read_tuple_head();

		if (arity >= 0) {
			elems = new OtpErlangObject[arity];

			for (int i = 0; i < arity; i++) {
				elems[i] = buf.read_any();
			}
		}
	}

	/**
	 * Get the arity of the tuple.
	 * 
	 * @return the number of elements contained in the tuple.
	 */
	public int arity() {
		return elems.length;
	}

	/**
	 * Get the specified element from the tuple.
	 * 
	 * @param i
	 *            the index of the requested element. Tuple elements are
	 *            numbered as array elements, starting at 0.
	 * 
	 * @return the requested element, of null if i is not a valid element index.
	 */
	public OtpErlangObject elementAt(int i) {
		if ((i >= arity()) || (i < 0)) {
			return null;
		}
		return elems[i];
	}

	/**
	 * Get all the elements from the tuple as an array.
	 * 
	 * @return an array containing all of the tuple's elements.
	 */
	public OtpErlangObject[] elements() {
		final OtpErlangObject[] res = new OtpErlangObject[arity()];
		System.arraycopy(elems, 0, res, 0, res.length);
		return res;
	}

	/**
	 * Get the string representation of the tuple.
	 * 
	 * @return the string representation of the tuple.
	 */
	@Override
	public String toString() {
		int i;
		final StringBuffer s = new StringBuffer();
		final int arity = elems.length;

		s.append("{");

		for (i = 0; i < arity; i++) {
			if (i > 0) {
				s.append(",");
			}
			s.append(elems[i].toString());
		}

		s.append("}");

		return s.toString();
	}

	/**
	 * Convert this tuple to the equivalent Erlang external representation.
	 * 
	 * @param buf
	 *            an output stream to which the encoded tuple should be written.
	 */
	@Override
	public void encode(OtpOutputStream buf) {
		final int arity = elems.length;

		buf.write_tuple_head(arity);

		for (int i = 0; i < arity; i++) {
			buf.write_any(elems[i]);
		}
	}

	/**
	 * Determine if two tuples are equal. Tuples are equal if they have the same
	 * arity and all of the elements are equal.
	 * 
	 * @param o
	 *            the tuple to compare to.
	 * 
	 * @return true if the tuples have the same arity and all the elements are
	 *         equal.
	 */
	@Override
	public boolean equals(Object o) {
		if (!(o instanceof OtpErlangTuple)) {
			return false;
		}

		final OtpErlangTuple t = (OtpErlangTuple) o;
		final int a = this.arity();

		if (a != t.arity()) {
			return false;
		}

		for (int i = 0; i < a; i++) {
			if (!elems[i].equals(t.elems[i])) {
				return false; // early exit
			}
		}

		return true;
	}

	@Override
	public Object clone() {
		final OtpErlangTuple newTuple = (OtpErlangTuple) (super.clone());
		newTuple.elems = elems.clone();
		return newTuple;
	}
}
