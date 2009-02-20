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

	private static final OtpErlangObject[] NO_ELEMENTS = new OtpErlangObject[0];

	private OtpErlangObject[] elems = NO_ELEMENTS;
	private OtpErlangObject lastTail = null;

	// TODO should we provide an iterator() ?

	/**
	 * Create an empty list.
	 */
	public OtpErlangList() {
		elems = NO_ELEMENTS;
	}

	/**
	 * Create a list of characters.
	 * 
	 * @param str
	 *            the characters from which to create the list.
	 */
	public OtpErlangList(final String str) {
		if (str != null) {
			final int len = str.length();
			if (len > 0) {
				elems = new OtpErlangObject[len];
				for (int i = 0; i < len; i++) {
					elems[i] = new OtpErlangChar(str.charAt(i));
				}
			}
		}
	}

	/**
	 * Create a list containing one element.
	 * 
	 * @param elem
	 *            the elememet to make the list from.
	 */
	public OtpErlangList(final OtpErlangObject elem) {
		elems = new OtpErlangObject[] { elem };
	}

	/**
	 * Create a list from an array of arbitrary Erlang terms.
	 * 
	 * @param elems
	 *            the array of terms from which to create the list.
	 */
	public OtpErlangList(final OtpErlangObject[] elems) {
		this(elems, 0, elems.length);
	}

	public OtpErlangList(final OtpErlangObject[] elems, boolean proper) {
		this(elems, 0, elems.length, proper);
	}

	/**
	 * Create a list from an array of arbitrary Erlang terms. Tail can be
	 * specified, if not null, the list will not be proper.
	 * 
	 * @param elems
	 *            array of terms from which to create the list
	 * @param tail
	 * @throws OtpErlangException
	 */
	public OtpErlangList(final OtpErlangObject[] elems,
			final OtpErlangObject tail) throws OtpErlangException {
		this(elems, 0, elems.length);
		if (elems.length == 0 && tail != null) {
			throw new OtpErlangException("Bad list, empty head, non-empty tail");
		}
		this.lastTail = tail;
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
	public OtpErlangList(final OtpErlangObject[] elems, final int start,
			final int count, boolean proper) {
		if (elems != null && count > 0) {
			if (proper) {
				this.elems = new OtpErlangObject[count];
				System.arraycopy(elems, start, this.elems, 0, count);
			} else if (count > 1) {
				this.elems = new OtpErlangObject[count - 1];
				System.arraycopy(elems, start, this.elems, 0, count - 1);
				lastTail = elems[start + count - 1];
			}
		}
	}

	public OtpErlangList(final OtpErlangObject[] elems, final int start,
			final int count) {
		this(elems, start, count, true);
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
	public OtpErlangList(final OtpInputStream buf)
			throws OtpErlangDecodeException {
		elems = null;
		final int arity = buf.read_list_head();
		if (arity > 0) {
			elems = new OtpErlangObject[arity];
			for (int i = 0; i < arity; i++) {
				elems[i] = buf.read_any();
			}
			/* discard the terminating nil (empty list) or read tail */
			if (buf.peek() == OtpExternal.nilTag) {
				buf.read_nil();
			} else {
				lastTail = buf.read_any();
				// TODO Should we check whether tail is an empty list here?
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
	public OtpErlangObject elementAt(final int i) {
		if (i >= arity() || i < 0) {
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
			return NO_ELEMENTS;
		} else {
			final OtpErlangObject[] res = new OtpErlangObject[arity()];
			System.arraycopy(elems, 0, res, 0, res.length);
			return res;
		}
	}

	/**
	 * Get the string representation of the list.
	 * 
	 * @return the string representation of the list.
	 */

	public String toString() {
		return toString(0);
	}

	protected String toString(int start) {
		final StringBuffer s = new StringBuffer();
		s.append("[");

		for (int i = start; i < arity(); i++) {
			if (i > start) {
				s.append(",");
			}
			s.append(elems[i].toString());
		}
		if (lastTail != null) {
			s.append("|").append(lastTail.toString());
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

	public void encode(final OtpOutputStream buf) {
		encode(buf, 0);
	}

	protected void encode(final OtpOutputStream buf, int start) {
		final int arity = arity() - start;

		if (arity > 0) {
			buf.write_list_head(arity);

			for (int i = start; i < arity + start; i++) {
				buf.write_any(elems[i]);
			}
		}
		if (lastTail == null) {
			buf.write_nil();
		} else {
			buf.write_any(lastTail);
		}
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

	public boolean equals(final Object o) {
		if (!(o instanceof OtpErlangList)) {
			return false;
		}

		final OtpErlangList l = (OtpErlangList) o;
		final int a = arity();

		if (a != l.arity()) {
			return false;
		}

		for (int i = 0; i < a; i++) {
			if (!elementAt(i).equals(l.elementAt(i))) {
				return false; // early exit
			}
		}
		OtpErlangObject otherTail = l.getLastTail();
		if (lastTail == null && otherTail == null) {
			return true;
		}
		if (lastTail == null) {
			return false;
		}
		return lastTail.equals(l.getLastTail());
	}

	protected OtpErlangObject getLastTail() {
		return lastTail;
	}

	public Object clone() {
		final OtpErlangList newList = (OtpErlangList) super.clone();
		newList.elems = (OtpErlangObject[]) elements().clone();
		if (lastTail != null) {
			newList.lastTail = (OtpErlangObject) lastTail.clone();
		}
		return newList;
	}

	/**
	 * @return true if the list is proper, i.e. the tail is nil
	 */
	public boolean isProper() {
		return lastTail == null;
	}

	public OtpErlangObject getHead() {
		if (elems.length > 0) {
			return elems[0];
		}
		return null;
	}

	public OtpErlangObject getTail() {
		if (elems.length > 0) {
			if (elems.length == 1) {
				return lastTail;
			} else {
				return new SubList(this, 1);
			}
		}
		return null;
	}

	public OtpErlangObject getNthTail(int n) {
		if (elems.length >= n) {
			if (elems.length == n) {
				return lastTail;
			} else {
				return new SubList(this, n);
			}
		}
		return null;
	}

	public static class SubList extends OtpErlangList {
		private static final long serialVersionUID = OtpErlangList.serialVersionUID;

		private int start;
		private OtpErlangList parent;

		public SubList(OtpErlangList parent, int start) {
			this.parent = parent;
			this.start = start;
		}

		public int arity() {
			return parent.arity() - start;
		}

		public OtpErlangObject elementAt(int i) {
			return parent.elementAt(i + start);
		}

		public OtpErlangObject[] elements() {
			OtpErlangObject[] res = new OtpErlangObject[arity()];
			return parent.elements();
		}

		public boolean isProper() {
			return parent.isProper();
		}

		public OtpErlangObject getHead() {
			return parent.elementAt(start);
		}

		public OtpErlangObject getTail() {
			return new SubList(parent, start + 1);
		}

		public OtpErlangObject getNthTail(int n) {
			System.out.println(n + " " + start + " " + parent.arity());
			if (n + start == parent.arity()) {
				return parent.getLastTail();
			}
			return parent.getNthTail(n + start);
		}

		public String toString() {
			return parent.toString(start);
		}

		public void encode(OtpOutputStream stream) {
			parent.encode(stream, start);
		}

		protected OtpErlangObject getLastTail() {
			return parent.getLastTail();
		}

	}

}
