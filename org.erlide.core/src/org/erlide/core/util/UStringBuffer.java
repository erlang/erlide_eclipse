/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *******************************************************************************/
package org.erlide.core.util;

import java.io.Serializable;

/**
 * Provides a string buffer that is unsynchronized
 * 
 * 
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public class UStringBuffer implements Serializable, Cloneable {

	private static final long serialVersionUID = 1L;

	/**
	 * The backing array
	 */
	private char[] backing;

	/**
	 * The length of the data
	 */
	private int length;

	/**
	 * Kick of the string with an initial capacity
	 * 
	 * @param alen
	 *            the initial capacity
	 */
	public UStringBuffer(final int alen) {
		super();

		backing = new char[alen];
	}

	/**
	 * Start the buffer with a default length
	 */
	public UStringBuffer() {
		this(10);
	}

	/**
	 * Get the length of the buffer
	 * 
	 * @return
	 */
	public int getLength() {
		return length;
	}

	/**
	 * Get the capacity of the system
	 * 
	 * @return the capacity
	 */
	public int getCapacity() {
		return backing.length;
	}

	/**
	 * Clone the buffer
	 * 
	 * @return the cloned string buffer
	 * @throws CloneNotSupportedException
	 */
	@Override
	public Object clone() throws CloneNotSupportedException {
		super.clone();

		final UStringBuffer bf = new UStringBuffer(backing.length);
		System.arraycopy(backing, 0, bf.backing, 0, backing.length);
		bf.length = length;
		return bf;
	}

	/**
	 * Make sure there is plenty of room without allot of swaping
	 * 
	 * @param cap
	 *            the new capacity
	 */
	public void ensureCapacity(int cap) {
		if (backing.length < cap) {
			increaseCapacity(cap);
		}
	}

	/**
	 * increase the capacity of the system
	 * 
	 * @param newCap
	 *            the new capacity
	 */
	private void increaseCapacity(final int newCap) {

		final char[] newBacking = new char[newCap];
		System.arraycopy(backing, 0, newBacking, 0, length);
		backing = newBacking;

	}

	/**
	 * Set the length of the valid data in the system.
	 * 
	 * @param newLength
	 *            the new length
	 */
	public void setLength(final int newLength) {
		final int olen = backing.length;

		if (backing.length < newLength) {
			increaseCapacity(newLength);
		}

		if (newLength > length && length != olen) {
			for (int i = length; i < olen; i++) {
				backing[i] = '\0';
			}
		}

		length = newLength;
	}

	/**
	 * Get the char at the specified index
	 * 
	 * @param index
	 * @return
	 */
	public char charAt(final int index) {
		if (index >= length) {
			throw new StringIndexOutOfBoundsException(index);
		}

		return backing[index];
	}

	/**
	 * Set the char at the specified index
	 * 
	 * @param index
	 *            The index to set at
	 * @param chr
	 *            the specified character
	 */
	public void setCharAt(final int index, final char chr) {
		if (index >= length) {
			throw new StringIndexOutOfBoundsException(index);
		}
		backing[index] = chr;
	}

	/**
	 * Append an object of indeterminant type
	 * 
	 * @param obj
	 *            The object to append
	 * @return this string buffer
	 */
	public UStringBuffer append(Object obj) {
		return append(obj.toString());
	}

	/**
	 * Append a string to this buffer
	 * 
	 * @param string
	 *            the string to append
	 * @return this buffer
	 */
	public UStringBuffer append(String string) {
		if (null == string) {
			string = "null";
		}

		final int len = string.length();
		final int newLength = length + len;
		if (backing.length < newLength) {
			increaseCapacity(newLength);
		}
		string.getChars(0, len, backing, length);
		length = newLength;
		return this;
	}

	/**
	 * Append another buffer to this one
	 * 
	 * @param sb
	 *            the new string buffer
	 * @return this object
	 */
	public UStringBuffer append(UStringBuffer sb) {
		if (null == sb) {
			return append("null");
		}

		final int len = sb.length;
		final int newLength = length + len;
		if (backing.length < newLength) {
			increaseCapacity(newLength);
		}

		System.arraycopy(sb.backing, 0, backing, length, sb.length);

		length = newLength;

		return this;
	}

	/**
	 * Append the specified character array
	 * 
	 * @param cha
	 *            the character array
	 * @return this string buffer
	 */
	public UStringBuffer append(final char cha[]) {
		final int len = cha.length;
		final int newLength = length + len;
		if (backing.length < newLength) {
			increaseCapacity(newLength);
		}
		System.arraycopy(cha, 0, backing, length, len);
		length = newLength;
		return this;
	}

	/**
	 * Append the specified character array at the specified offset with the
	 * specified len
	 * 
	 * @param cha
	 *            the character array
	 * @param offset
	 *            the offset
	 * @param len
	 *            the lenght
	 * @return this string buffer
	 */
	public UStringBuffer append(char cha[], int offset, int len) {
		final int newLength = length + len;
		if (backing.length < newLength) {
			increaseCapacity(newLength);
		}
		System.arraycopy(cha, offset, backing, length, len);
		length = newLength;
		return this;
	}

	/**
	 * Append the boolean
	 * 
	 * @param b
	 *            the bolean to append
	 * @return this string buffer
	 */
	public UStringBuffer append(boolean b) {
		if (b) {
			final int newLength = length + 4;
			if (backing.length < newLength) {
				increaseCapacity(newLength);
			}
			backing[length++] = 't';
			backing[length++] = 'r';
			backing[length++] = 'u';
			backing[length++] = 'e';
		} else {
			final int newLength = length + 5;
			if (backing.length < newLength) {
				increaseCapacity(newLength);
			}
			backing[length++] = 'f';
			backing[length++] = 'a';
			backing[length++] = 'l';
			backing[length++] = 's';
			backing[length++] = 'e';
		}
		return this;
	}

	/**
	 * Sppend the specified character
	 * 
	 * @param c
	 *            the character to append
	 * @return this string buffer
	 */
	public UStringBuffer append(char c) {
		final int newLength = length + 1;
		if (backing.length < newLength) {
			increaseCapacity(newLength);
		}
		backing[length++] = c;
		return this;
	}

	/**
	 * Append the specified integer
	 * 
	 * @param i
	 *            the integer to append
	 * @return thjis string buffer
	 */
	public UStringBuffer append(int i) {
		return append(Integer.toString(i));
	}

	/**
	 * Append the specified long
	 * 
	 * @param l
	 *            the long
	 * @return this string buffer
	 */
	public UStringBuffer append(long l) {
		return append(Long.toString(l));
	}

	/**
	 * Append the specified float
	 * 
	 * @param f
	 *            the float
	 * @return this string buffer
	 */
	public UStringBuffer append(float f) {
		return append(Float.toString(f));
	}

	/**
	 * Append the specified double
	 * 
	 * @param d
	 *            the double
	 * @return this string buffer
	 */
	public UStringBuffer append(double d) {
		return append(Double.toString(d));
	}

	/**
	 * Delete the characters between the two points
	 * 
	 * @param start
	 *            the start location
	 * @param end
	 *            the end location
	 * @return this string buffer
	 */
	public UStringBuffer delete(int start, int end) {
		if (end > length) {
			end = length;
		}

		final int len = end - start;
		if (len > 0) {
			System
					.arraycopy(backing, start + len, backing, start, length -
							end);
			length -= len;
		}
		return this;
	}

	/**
	 * Delete the character at the specified index
	 * 
	 * @param index
	 *            the index to delete at
	 * @return this string buffer
	 */
	public UStringBuffer deleteCharAt(int index) {
		if (index >= length) {
			throw new StringIndexOutOfBoundsException();
		}

		System
				.arraycopy(backing, index + 1, backing, index, length - index -
						1);
		length--;
		return this;
	}

	/**
	 * Replace the start to end with the specified string
	 * 
	 * @param start
	 *            the replacement string
	 * @param end
	 *            the end value
	 * @param string
	 *            The start value
	 * @return this string buffer
	 */
	public UStringBuffer replace(int start, int end, String string) {
		if (end > length) {
			end = length;
		}

		final int len = string.length();
		final int newLength = length + len - (end - start);
		if (newLength > backing.length) {
			increaseCapacity(newLength);
		}

		System.arraycopy(backing, end, backing, start + len, length - end);
		string.getChars(0, len, backing, start);
		length = newLength;
		return this;
	}

	/**
	 * Returns a new sub string starting at start and including the whole buffer
	 * 
	 * @param start
	 *            the start location
	 * @return a new string buffer
	 */
	public UStringBuffer substring(int start) {
		return substring(start, length);
	}

	/**
	 * Get a substring of this buffer starting at start and ending at end.
	 * 
	 * @param start
	 *            the start location
	 * @param end
	 *            the end location
	 * @return a new String buffer
	 */
	public UStringBuffer substring(int start, int end) {
		if (end > length) {
			throw new StringIndexOutOfBoundsException(end);
		}
		if (start > end) {
			throw new StringIndexOutOfBoundsException(end - start);
		}

		final int nlen = end - start;
		final UStringBuffer tmp = new UStringBuffer(nlen);
		System.arraycopy(backing, start, tmp.backing, 0, nlen);
		tmp.length = nlen;
		return tmp;
	}

	/**
	 * insert the specified array into the buffer
	 * 
	 * @param index
	 *            the index to insert at
	 * @param cha
	 *            the inserted array
	 * @param offset
	 *            the offset into the inserted array
	 * @param len
	 *            the length to insert from the inserted array
	 * @return this string buffer
	 */
	public UStringBuffer insert(int index, char cha[], int offset, int len) {
		if (index > length) {
			throw new StringIndexOutOfBoundsException();
		}
		if ((offset + len < 0) || (offset + len > cha.length)) {
			throw new StringIndexOutOfBoundsException(offset);
		}
		if (len < 0) {
			throw new StringIndexOutOfBoundsException(len);
		}
		final int newLength = length + len;

		if (backing.length < newLength) {
			increaseCapacity(newLength);
		}

		System.arraycopy(backing, index, backing, index + len, length - index);
		System.arraycopy(cha, offset, backing, index, len);
		length = newLength;
		return this;
	}

	/**
	 * insert the object into the buffer at the specified offset
	 * 
	 * @param offset
	 *            the ofset to insert
	 * @param obj
	 *            the object to inset
	 * @return this string buffer
	 */
	public UStringBuffer insert(int offset, Object obj) {
		return insert(offset, obj.toString());
	}

	/**
	 * insert the specified string at the specified offset
	 * 
	 * @param offset
	 *            the offset to insert at
	 * @param string
	 *            the string to insert
	 * @return this string buffer
	 */
	public UStringBuffer insert(int offset, String string) {
		if ((offset < 0) || (offset > length)) {
			throw new StringIndexOutOfBoundsException();
		}

		if (string == null) {
			string = "null";
		}
		final int len = string.length();
		final int newLength = length + len;
		if (backing.length < newLength) {
			increaseCapacity(newLength);
		}
		System.arraycopy(backing, offset, backing, offset + len, length -
				offset);
		string.getChars(0, len, backing, offset);
		length = newLength;
		return this;
	}

	/**
	 * insert the character array into the buffer
	 * 
	 * @param offset
	 *            the offset into the buffer
	 * @param cha
	 *            the character array
	 * @return this string buffer
	 */
	public UStringBuffer insert(int offset, char cha[]) {
		return insert(offset, cha, 0, cha.length);
	}

	/**
	 * Insert the boolean into the buffer
	 * 
	 * @param offset
	 *            the offset to insert at
	 * @param b
	 *            the boolean to insert
	 * @return this string buffer
	 */
	public UStringBuffer insert(int offset, boolean b) {
		if (b) {
			final int newLength = length + 4;
			if (backing.length < newLength) {
				increaseCapacity(newLength);
			}
			System.arraycopy(backing, offset, backing, offset + 4, length -
					offset);
			length = newLength;
			backing[offset++] = 't';
			backing[offset++] = 'r';
			backing[offset++] = 'u';
			backing[offset++] = 'e';
		} else {
			final int newLength = length + 5;
			if (backing.length < newLength) {
				increaseCapacity(newLength);
			}
			System.arraycopy(backing, offset, backing, offset + 5, length -
					offset);
			length = newLength;
			backing[offset++] = 'f';
			backing[offset++] = 'a';
			backing[offset++] = 'l';
			backing[offset++] = 's';
			backing[offset++] = 'e';
		}
		return this;
	}

	/**
	 * insert a character into the buffer
	 * 
	 * @param offset
	 *            the offset to insert at
	 * @param c
	 *            the character to insert
	 * @return this string buffer
	 */
	public UStringBuffer insert(int offset, char c) {
		final int newLength = length + 1;
		if (backing.length < newLength) {
			increaseCapacity(newLength);
		}
		System.arraycopy(backing, offset, backing, offset + 1, length - offset);
		backing[offset] = c;
		length = newLength;
		return this;
	}

	/**
	 * Insert the specified integer into the array
	 * 
	 * @param offset
	 *            the offset to insert at
	 * @param i
	 *            The integer to insert
	 * @return this string buffer
	 */
	public UStringBuffer insert(int offset, int i) {
		return insert(offset, Integer.toString(i));
	}

	/**
	 * Insert the specified long into the array
	 * 
	 * @param offset
	 *            the insret point
	 * @param l
	 *            the long to insert
	 * @return this string buffer
	 */
	public UStringBuffer insert(int offset, long l) {
		return insert(offset, Long.toString(l));
	}

	/**
	 * Insert the specified float into the array
	 * 
	 * @param offset
	 *            the offset to insert at
	 * @param f
	 *            the float to insert
	 * @return this string buffer
	 */
	public UStringBuffer insert(int offset, float f) {
		return insert(offset, Float.toString(f));
	}

	/**
	 * Insert the specified double into the buffer
	 * 
	 * @param offset
	 *            the offset to insert at
	 * @param d
	 *            the double to insert
	 * @return this string buffer
	 */
	public UStringBuffer insert(int offset, double d) {
		return insert(offset, Double.toString(d));
	}

	/**
	 * Find the index of the specified strnig
	 * 
	 * @param string
	 *            the string to find the index of
	 * @return this position of the first match of -1 if none was found
	 */
	public int indexOf(String string) {
		return indexOf(string, 0);
	}

	/**
	 * Find the index of the first system match
	 * 
	 * @param string
	 *            the string to match
	 * @param fromIndex
	 *            the index to start at
	 * @return this position of the first match of -1 if none was found
	 */
	public int indexOf(String string, int fromIndex) {
		if (fromIndex >= length || string.length() > (length - fromIndex)) {
			return -1;
		}

		final char[] cha = string.toCharArray();

		// It cant match if its shorter then length
		for (int i = fromIndex; i >= 0; i++) {
			if (backing[i] == cha[0] && checkMatch(cha, i)) {
				return i;
			}

		}
		return -1;
	}

	/**
	 * Find the last match for the string
	 * 
	 * @param string
	 *            the string to match
	 * @return this position of the last match of -1 if none was found
	 */
	public int lastIndexOf(String string) {
		return lastIndexOf(string, length);
	}

	/**
	 * Find the last index of the specified string
	 * 
	 * @param string
	 *            the string to find
	 * @param fromIndex
	 *            the start location
	 * @return this position of the last match of -1 if none was found
	 */
	public int lastIndexOf(String string, int fromIndex) {
		if (fromIndex > length || string.length() > length ||
				string.length() > fromIndex) {
			return -1;
		}

		final char[] cha = string.toCharArray();

		// It cant match if its shorter then length
		for (int i = (fromIndex - cha.length); i >= 0; i--) {
			if (backing[i] == cha[0] && checkMatch(cha, i)) {
				return i;
			}

		}
		return -1;
	}

	/**
	 * Check to see if a match exists at the specified location
	 * 
	 * @param cha
	 *            the array to check
	 * @param loc
	 *            the location to start at
	 * @return boolean indicating success
	 */
	private boolean checkMatch(char[] cha, int loc) {
		for (int j = loc, k = 0; j < length && k < cha.length; j++, k++) {
			if (backing[j] != cha[k]) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Reverse this buffer
	 * 
	 * @return this string buffer
	 */
	public UStringBuffer reverse() {
		char temp;
		if (0 != length) {
			final int half = (((length % 2) == 0) ? (length / 2) - 1
					: (length / 2));

			for (int j = 0, i = length - 1; j <= half; j++, i--) {
				temp = backing[j];
				backing[j] = backing[i];
				backing[i] = temp;
			}
		}
		return this;
	}

	/**
	 * Convert this item to a string
	 * 
	 * @return the string representing this object
	 */
	@Override
	public String toString() {
		return new String(backing, 0, length);
	}

	/**
	 * Get the char array
	 * 
	 * @return the char array
	 */
	public char[] toCharArray() {
		final char[] tmp = new char[length];

		System.arraycopy(backing, 0, tmp, 0, length);
		return tmp;
	}

	/**
	 * Convert this item to a string buffer
	 * 
	 * @return the string buffer
	 */
	public StringBuffer toStringBuffer() {
		final StringBuffer tmp = new StringBuffer(length);
		tmp.append(backing, 0, length);

		return tmp;
	}

	/**
	 * Reduces the length of the buffer by 1
	 * 
	 */
	public void decrementLength() {
		length--;
	}

}