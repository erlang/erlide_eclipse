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
 * Provides a Java representation of Erlang lists. Lists are created from zero
 * or more arbitrary Erlang terms.
 * 
 * <p>
 * The arity of the list is the number of elements it contains.
 */
public class OtpErlangList extends OtpErlangObject implements Serializable,
		Cloneable {

	// don't change this!
	static final long serialVersionUID = 5999112769036676548L;

	private OtpErlangObject[] elems = null;

	// todo: use a linked structure to make a proper list with append,
	// car, cdr etc methods. The current representation is essensially the
	// same as for tuples except that empty lists are allowed.
	// private OtpErlangObject car = null;
	// private OtpErlangObject cdr = null;
	// int arity;

	/**
	 * Create an empty list.
	 */
	public OtpErlangList() {
		this.elems = null; // empty list
	}

	/**
	 * Create a list of characters.
	 * 
	 * @param str
	 *            the characters from which to create the list.
	 */
	public OtpErlangList(String str) {
		int len = 0;

		if (str != null) {
			len = str.length();
		}

		if (len > 0) {
			this.elems = new OtpErlangObject[len];

			for (int i = 0; i < len; i++) {
				this.elems[i] = new OtpErlangChar(str.charAt(i));
			}
		}
	}

	/**
	 * Create a list containing one element.
	 * 
	 * @param elem
	 *            the elememet to make the list from.
	 */
	public OtpErlangList(OtpErlangObject elem) {
		this.elems = new OtpErlangObject[1];
		elems[0] = elem;
	}

	/**
	 * Create a list from an array of arbitrary Erlang terms.
	 * 
	 * @param elems
	 *            the array of terms from which to create the list.
	 */
	public OtpErlangList(OtpErlangObject[] elems) {
		this(elems, 0, elems.length);
	}

	/**
	 * Create a list from an array of arbitrary Erlang terms.
	 * 
	 * @param elems
	 *            the array of terms from which to create the list.
	 * @param start
	 *            the offset of the first term to insert.
	 * @param count
	 *            the number of terms to insert.
	 */
	public OtpErlangList(OtpErlangObject[] elems, int start, int count) {
		if ((elems != null) && (count > 0)) {
			this.elems = new OtpErlangObject[count];
			System.arraycopy(elems, start, this.elems, 0, count);
		}
	}

	/**
	 * Create a list from a stream containing an list encoded in Erlang external
	 * format.
	 * 
	 * @param buf
	 *            the stream containing the encoded list.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the buffer does not contain a valid external
	 *                representation of an Erlang list.
	 */
	public OtpErlangList(OtpInputStream buf) throws OtpErlangDecodeException {
		this.elems = null;

		final int arity = buf.read_list_head();

		if (arity > 0) {
			this.elems = new OtpErlangObject[arity];

			for (int i = 0; i < arity; i++) {
				elems[i] = buf.read_any();
			}

			/* discard the terminating nil (empty list) */
			try {
				buf.read_nil();
			} catch (final OtpErlangDecodeException e) {
				if (e.getMessage().startsWith("Not valid nil tag:")) {
					throw new OtpErlangDecodeException("Non proper list");
				} else {
					throw e;
				}
			}
		}
	}

	/**
	 * Get the arity of the list.
	 * 
	 * @return the number of elements contained in the list.
	 */
	public int arity() {
		if (elems == null) {
			return 0;
		} else {
			return elems.length;
		}
	}

	/**
	 * Get the specified element from the list.
	 * 
	 * @param i
	 *            the index of the requested element. List elements are numbered
	 *            as array elements, starting at 0.
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
	 * Get all the elements from the list as an array.
	 * 
	 * @return an array containing all of the list's elements.
	 */
	public OtpErlangObject[] elements() {
		if (arity() == 0) {
			return null;
		} else {
			final OtpErlangObject[] res = new OtpErlangObject[arity()];
			System.arraycopy(this.elems, 0, res, 0, res.length);
			return res;
		}
	}

	/**
	 * Get the string representation of the list.
	 * 
	 * @return the string representation of the list.
	 */
	@Override
	public String toString() {
		final StringBuffer s = new StringBuffer();
		final int arity = arity();

		s.append("[");

		for (int i = 0; i < arity; i++) {
			if (i > 0) {
				s.append(",");
			}
			s.append(elems[i].toString());
		}

		s.append("]");

		return s.toString();
	}

	/**
	 * Convert this list to the equivalent Erlang external representation. Note
	 * that this method never encodes lists as strings, even when it is possible
	 * to do so.
	 * 
	 * @param buf
	 *            An output stream to which the encoded list should be written.
	 * 
	 */
	@Override
	public void encode(OtpOutputStream buf) {
		final int arity = arity();

		if (arity > 0) {
			buf.write_list_head(arity);

			for (int i = 0; i < arity; i++) {
				buf.write_any(elems[i]);
			}
		}

		buf.write_nil();
	}

	/**
	 * Determine if two lists are equal. Lists are equal if they have the same
	 * arity and all of the elements are equal.
	 * 
	 * @param o
	 *            the list to compare to.
	 * 
	 * @return true if the lists have the same arity and all the elements are
	 *         equal.
	 */
	@Override
	public boolean equals(Object o) {
		if (!(o instanceof OtpErlangList)) {
			return false;
		}

		final OtpErlangList l = (OtpErlangList) o;
		final int a = this.arity();

		if (a != l.arity()) {
			return false;
		}

		for (int i = 0; i < a; i++) {
			if (!this.elems[i].equals(l.elems[i])) {
				return false; // early exit
			}
		}

		return true;
	}

	@Override
	public Object clone() {
		final OtpErlangList newList = (OtpErlangList) (super.clone());
		newList.elems = elems.clone();
		return newList;
	}
}
