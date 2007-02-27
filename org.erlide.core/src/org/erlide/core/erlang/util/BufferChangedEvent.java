/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.core.erlang.util;

import java.util.EventObject;

/**
 * A buffer changed event describes how a buffer has changed. These events are
 * used in <code>IBufferChangedListener</code> notifications.
 * <p>
 * For text insertions, <code>getOffset</code> is the offset of the first
 * inserted character, <code>getText</code> is the inserted text, and
 * <code>getLength</code> is 0.
 * </p>
 * <p>
 * For text removals, <code>getOffset</code> is the offset of the first
 * removed character, <code>getText</code> is <code>null</code>, and
 * <code>getLength</code> is the length of the text that was removed.
 * </p>
 * <p>
 * For replacements (including <code>IBuffer.setContents</code>),
 * <code>getOffset</code> is the offset of the first replaced character,
 * <code>getText</code> is the replacement text, and <code>getLength</code>
 * is the length of the original text that was replaced.
 * </p>
 * <p>
 * When a buffer is closed, <code>getOffset</code> is 0,<code>getLength</code>
 * is 0, and <code>getText</code> is <code>null</code>.
 * </p>
 * <p>
 * This class is not intended to be instantiated or subclassed by clients.
 * Instances of this class are automatically created by the Erlang model.
 * </p>
 * 
 * @see IBuffer
 */
public class BufferChangedEvent extends EventObject {

	private static final long serialVersionUID = 1L;

	/**
	 * The length of text that has been modified in the buffer.
	 */
	private int fLength;

	/**
	 * The offset into the buffer where the modification took place.
	 */
	private int fOffset;

	/**
	 * The text that was modified.
	 */
	private String fText;

	/**
	 * Creates a new buffer changed event indicating that the given buffer has
	 * changed.
	 * 
	 * @param buffer
	 *            the given buffer
	 * @param offset
	 *            the given offset
	 * @param length
	 *            the given length
	 * @param text
	 *            the given text
	 */
	public BufferChangedEvent(IBuffer buffer, int offset, int length,
			String text) {
		super(buffer);
		fOffset = offset;
		fLength = length;
		fText = text;
	}

	/**
	 * Returns the buffer which has changed.
	 * 
	 * @return the buffer affected by the change
	 */
	public IBuffer getBuffer() {
		return (IBuffer) source;
	}

	/**
	 * Returns the length of text removed or replaced in the buffer, or 0 if
	 * text has been inserted into the buffer.
	 * 
	 * @return the length of the original text fragment modified by the buffer
	 *         change ( <code> 0 </code> in case of insertion).
	 */
	public int getLength() {
		return fLength;
	}

	/**
	 * Returns the index of the first character inserted, removed, or replaced
	 * in the buffer.
	 * 
	 * @return the source offset of the textual manipulation in the buffer
	 */
	public int getOffset() {
		return fOffset;
	}

	/**
	 * Returns the text that was inserted, the replacement text, or
	 * <code>null</code> if text has been removed.
	 * 
	 * @return the text corresponding to the buffer change (<code> null </code>
	 *         in case of deletion).
	 */
	public String getText() {
		return fText;
	}
}
