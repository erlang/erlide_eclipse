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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.SafeRunner;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.IErlModelStatusConstants;
import org.erlide.core.erlang.IOpenable;
import org.erlide.core.erlang.internal.ErlElement;

/**
 * @see IBuffer
 * @author Vlad
 */
public class Buffer implements IBuffer {

	/**
	 * Field file
	 */
	protected IFile fFile;

	/**
	 * Field flags
	 */
	protected int flags;

	/**
	 * Field contents
	 */
	protected char[] contents;

	/**
	 * Field changeListeners
	 */
	protected ArrayList<IBufferChangedListener> changeListeners;

	/**
	 * Field owner
	 */
	protected IOpenable fOwner;

	/**
	 * Field gapStart
	 */
	protected int gapStart = -1;

	/**
	 * Field gapEnd
	 */
	protected int gapEnd = -1;

	/**
	 * Field lock
	 */
	protected Object lock = new Object();

	/**
	 * Field F_HAS_UNSAVED_CHANGES (value is 1)
	 */
	protected static final int F_HAS_UNSAVED_CHANGES = 1;

	/**
	 * Field F_IS_READ_ONLY (value is 2)
	 */
	protected static final int F_IS_READ_ONLY = 2;

	/**
	 * Field F_IS_CLOSED (value is 4)
	 */
	protected static final int F_IS_CLOSED = 4;

	/**
	 * Creates a new buffer on an underlying resource.
	 * 
	 * @param file
	 *            IFile
	 * @param owner
	 *            IOpenable
	 * @param readOnly
	 *            boolean
	 */
	protected Buffer(IFile file, IOpenable owner, boolean readOnly) {
		fFile = file;
		fOwner = owner;
		if (file == null) {
			setReadOnly(readOnly);
		}
	}

	/**
	 * @param listener
	 *            IBufferChangedListener
	 * @see IBuffer
	 */
	public void addBufferChangedListener(IBufferChangedListener listener) {
		if (changeListeners == null) {
			changeListeners = new ArrayList<IBufferChangedListener>(5);
		}
		if (!changeListeners.contains(listener)) {
			changeListeners.add(listener);
		}
	}

	/**
	 * Append the <code>text</code> to the actual content, the gap is moved to
	 * the end of the <code>text</code>.
	 * 
	 * @param text
	 *            char[]
	 * @see org.erlide.core.erlang.util.IBuffer#append(char[])
	 */
	public void append(char[] text) {
		if (!isReadOnly()) {
			if (text == null || text.length == 0) {
				return;
			}
			final int length = getLength();
			synchronized (lock) {
				if (contents == null) {
					return;
				}
				moveAndResizeGap(length, text.length);
				System.arraycopy(text, 0, contents, length, text.length);
				gapStart += text.length;
				flags |= F_HAS_UNSAVED_CHANGES;
			}
			notifyChanged(new BufferChangedEvent(this, length, 0, new String(
					text)));
		}
	}

	/**
	 * Append the <code>text</code> to the actual content, the gap is moved to
	 * the end of the <code>text</code>.
	 * 
	 * @param text
	 *            String
	 * @see org.erlide.core.erlang.util.IBuffer#append(String)
	 */
	public void append(String text) {
		if (text == null) {
			return;
		}
		this.append(text.toCharArray());
	}

	/**
	 * @see IBuffer
	 */
	public void close() {
		BufferChangedEvent event = null;
		synchronized (lock) {
			if (isClosed()) {
				return;
			}
			event = new BufferChangedEvent(this, 0, 0, null);
			contents = null;
			flags |= F_IS_CLOSED;
		}
		notifyChanged(event); // notify outside of synchronized block
		changeListeners = null;
	}

	/**
	 * @param position
	 *            int
	 * @return char
	 * @see IBuffer
	 */
	public char getChar(int position) {
		synchronized (lock) {
			if (contents == null) {
				return Character.MIN_VALUE;
			}
			if (position < gapStart) {
				return contents[position];
			}
			final int gapLength = gapEnd - gapStart;
			return contents[position + gapLength];
		}
	}

	/**
	 * @return char[]
	 * @see IBuffer
	 */
	public char[] getCharacters() {
		synchronized (lock) {
			if (contents == null) {
				return null;
			}
			if (gapStart < 0) {
				return contents;
			}
			final int length = contents.length;
			final char[] newContents = new char[length - gapEnd + gapStart];
			System.arraycopy(contents, 0, newContents, 0, gapStart);
			System.arraycopy(contents, gapEnd, newContents, gapStart, length
					- gapEnd);
			return newContents;
		}
	}

	/**
	 * @return String
	 * @see IBuffer
	 */
	public String getContents() {
		final char[] chars = this.getCharacters();
		if (chars == null) {
			return null;
		}
		return new String(chars);
	}

	/**
	 * @return int
	 * @see IBuffer
	 */
	public int getLength() {
		synchronized (lock) {
			if (contents == null) {
				return -1;
			}
			final int length = gapEnd - gapStart;
			return contents.length - length;
		}
	}

	/**
	 * @return IOpenable
	 * @see IBuffer
	 */
	public IOpenable getOwner() {
		return fOwner;
	}

	/**
	 * @param offset
	 *            int
	 * @param length
	 *            int
	 * @return String
	 * @see IBuffer
	 */
	public String getText(int offset, int length) {
		synchronized (lock) {
			if (contents == null) {
				return ""; //$NON-NLS-1$
			}
			if (offset + length < gapStart) {
				return new String(contents, offset, length);
			}
			if (gapStart < offset) {
				final int gapLength = gapEnd - gapStart;
				return new String(contents, offset + gapLength, length);
			}
			final StringBuffer buf = new StringBuffer();
			buf.append(contents, offset, gapStart - offset);
			buf.append(contents, gapEnd, offset + length - gapStart);
			return buf.toString();
		}
	}

	/**
	 * @return IResource
	 * @see IBuffer
	 */
	public IResource getUnderlyingResource() {
		return fFile;
	}

	/**
	 * @return boolean
	 * @see IBuffer
	 */
	public boolean hasUnsavedChanges() {
		return (flags & F_HAS_UNSAVED_CHANGES) != 0;
	}

	/**
	 * @return boolean
	 * @see IBuffer
	 */
	public boolean isClosed() {
		return (flags & F_IS_CLOSED) != 0;
	}

	/**
	 * @return boolean
	 * @see IBuffer
	 */
	public boolean isReadOnly() {
		return (flags & F_IS_READ_ONLY) != 0;
	}

	/**
	 * Moves the gap to location and adjust its size to the anticipated change
	 * size. The size represents the expected range of the gap that will be
	 * filled after the gap has been moved. Thus the gap is resized to actual
	 * size + the specified size and moved to the given position.
	 * 
	 * @param position
	 *            int
	 * @param size
	 *            int
	 */
	protected void moveAndResizeGap(int position, int size) {
		char[] content = null;
		final int oldSize = gapEnd - gapStart;
		if (size < 0) {
			if (oldSize > 0) {
				content = new char[contents.length - oldSize];
				System.arraycopy(contents, 0, content, 0, gapStart);
				System.arraycopy(contents, gapEnd, content, gapStart,
						content.length - gapStart);
				contents = content;
			}
			gapStart = gapEnd = position;
			return;
		}
		content = new char[contents.length + (size - oldSize)];
		final int newGapStart = position;
		final int newGapEnd = newGapStart + size;
		if (oldSize == 0) {
			System.arraycopy(contents, 0, content, 0, newGapStart);
			System.arraycopy(contents, newGapStart, content, newGapEnd,
					content.length - newGapEnd);
		} else if (newGapStart < gapStart) {
			final int delta = gapStart - newGapStart;
			System.arraycopy(contents, 0, content, 0, newGapStart);
			System.arraycopy(contents, newGapStart, content, newGapEnd, delta);
			System.arraycopy(contents, gapEnd, content, newGapEnd + delta,
					contents.length - gapEnd);
		} else {
			final int delta = newGapStart - gapStart;
			System.arraycopy(contents, 0, content, 0, gapStart);
			System.arraycopy(contents, gapEnd, content, gapStart, delta);
			System.arraycopy(contents, gapEnd + delta, content, newGapEnd,
					content.length - newGapEnd);
		}
		contents = content;
		gapStart = newGapStart;
		gapEnd = newGapEnd;
	}

	/**
	 * Notify the listeners that this buffer has changed. To avoid deadlock,
	 * this should not be called in a synchronized block.
	 * 
	 * @param event
	 *            BufferChangedEvent
	 */
	protected void notifyChanged(final BufferChangedEvent event) {
		if (changeListeners != null) {
			for (int i = 0, size = changeListeners.size(); i < size; ++i) {
				final IBufferChangedListener listener = changeListeners.get(i);
				SafeRunner.run(new ISafeRunnable() {

					public void handleException(Throwable exception) {
						Util
								.log(exception,
										"Exception occurred in listener of buffer change notification"); //$NON-NLS-1$
					}

					public void run() throws Exception {
						listener.bufferChanged(event);
					}
				});

			}
		}
	}

	/**
	 * @param listener
	 *            IBufferChangedListener
	 * @see IBuffer
	 */
	public void removeBufferChangedListener(IBufferChangedListener listener) {
		if (changeListeners != null) {
			changeListeners.remove(listener);
			if (changeListeners.size() == 0) {
				changeListeners = null;
			}
		}
	}

	/**
	 * Replaces <code>length</code> characters starting from
	 * <code>position</code> with <code>text<code>.
	 * After that operation, the gap is placed at the end of the 
	 * inserted <code>text</code>.
	 * @param position int
	 * @param length int
	 * @param text char[]
	 * @see org.erlide.core.erlang.util.IBuffer#replace(int, int, char[])
	 */
	public void replace(int position, int length, char[] text) {
		if (!isReadOnly()) {
			final int textLength = (text == null) ? 0 : text.length;
			synchronized (lock) {
				if (contents == null) {
					return;
				}

				// move gap
				moveAndResizeGap(position + length, textLength - length);

				// overwrite
				final int min = Math.min(textLength, length);
				if (min > 0) {
					System.arraycopy(text, 0, contents, position, min);
				}
				if (length > textLength) {
					// enlarge the gap
					gapStart -= length - textLength;
				} else if (textLength > length) {
					// shrink gap
					gapStart += textLength - length;
					System.arraycopy(text, 0, contents, position, textLength);
				}
				flags |= F_HAS_UNSAVED_CHANGES;
			}
			String string = null;
			if (textLength > 0) {
				string = new String(text);
			}
			notifyChanged(new BufferChangedEvent(this, position, length, string));
		}
	}

	/**
	 * Replaces <code>length</code> characters starting from
	 * <code>position</code> with <code>text<code>.
	 * After that operation, the gap is placed at the end of the 
	 * inserted <code>text</code>.
	 * @param position int
	 * @param length int
	 * @param text String
	 * @see org.erlide.core.erlang.util.IBuffer#replace(int, int, String)
	 */
	public void replace(int position, int length, String text) {
		this.replace(position, length, (text == null) ? null : text
				.toCharArray());
	}

	/**
	 * @param progress
	 *            IProgressMonitor
	 * @param force
	 *            boolean
	 * @throws ErlModelException
	 * @see IBuffer
	 */
	public void save(IProgressMonitor progress, boolean force)
			throws ErlModelException {

		// determine if saving is required
		if (isReadOnly() || fFile == null) {
			return;
		}
		if (!hasUnsavedChanges()) {
			return;
		}

		// use a platform operation to update the resource contents
		try {
			String encoding = null;
			try {
				encoding = fFile.getCharset();
			} catch (final CoreException ce) {
				// use no encoding
			}
			final String stringContents = this.getContents();
			if (stringContents == null) {
				return;
			}
			final byte[] bytes = (encoding == null) ? stringContents.getBytes()
					: stringContents.getBytes(encoding);
			final ByteArrayInputStream stream = new ByteArrayInputStream(bytes);

			if (fFile.exists()) {
				fFile
						.setContents(stream, force ? IResource.FORCE
								| IResource.KEEP_HISTORY
								: IResource.KEEP_HISTORY, null);
			} else {
				fFile.create(stream, force, null);
			}
		} catch (final IOException e) {
			throw new ErlModelException(e,
					IErlModelStatusConstants.IO_EXCEPTION);
		} catch (final CoreException e) {
			throw new ErlModelException(e);
		}

		// the resource no longer has unsaved changes
		flags &= ~(F_HAS_UNSAVED_CHANGES);
	}

	/**
	 * @param newContents
	 *            char[]
	 * @see IBuffer
	 */
	public void setContents(char[] newContents) {
		// allow special case for first initialization
		// after creation by buffer factory
		if (contents == null) {
			synchronized (lock) {
				contents = newContents;
				flags &= ~(F_HAS_UNSAVED_CHANGES);
			}
			return;
		}

		if (!isReadOnly()) {
			String string = null;
			if (newContents != null) {
				string = new String(newContents);
			}
			synchronized (lock) {
				if (contents == null) {
					return; // ignore if buffer is closed (as per spec)
				}
				contents = newContents;
				flags |= F_HAS_UNSAVED_CHANGES;
				gapStart = -1;
				gapEnd = -1;
			}
			final BufferChangedEvent event = new BufferChangedEvent(this, 0,
					this.getLength(), string);
			notifyChanged(event);
		}
	}

	/**
	 * @param newContents
	 *            String
	 * @see IBuffer
	 */
	public void setContents(String newContents) {
		this.setContents(newContents.toCharArray());
	}

	/**
	 * Sets this <code>Buffer</code> to be read only.
	 * 
	 * @param readOnly
	 *            boolean
	 */
	protected final void setReadOnly(boolean readOnly) {
		if (readOnly) {
			flags |= F_IS_READ_ONLY;
		} else {
			flags &= ~(F_IS_READ_ONLY);
		}
	}

	/**
	 * Method toString
	 * 
	 * @return String
	 */
	@Override
	public String toString() {
		final StringBuffer buffer = new StringBuffer();
		buffer
				.append("Owner: " + ((ErlElement) fOwner).toStringWithAncestors()); //$NON-NLS-1$
		buffer.append("\nHas unsaved changes: " + this.hasUnsavedChanges()); //$NON-NLS-1$
		buffer.append("\nIs readonly: " + this.isReadOnly()); //$NON-NLS-1$
		buffer.append("\nIs closed: " + this.isClosed()); //$NON-NLS-1$
		buffer.append("\nContents:\n"); //$NON-NLS-1$
		final char[] charContents = this.getCharacters();
		if (charContents == null) {
			buffer.append("<null>"); //$NON-NLS-1$
		} else {
			final int length = charContents.length;
			for (int i = 0; i < length; i++) {
				final char c = charContents[i];
				switch (c) {
				case '\n':
					buffer.append("\\n\n"); //$NON-NLS-1$
					break;
				case '\r':
					if (i < length - 1 && contents[i + 1] == '\n') {
						buffer.append("\\r\\n\n"); //$NON-NLS-1$
						i++;
					} else {
						buffer.append("\\r\n"); //$NON-NLS-1$
					}
					break;
				default:
					buffer.append(c);
					break;
				}
			}
		}
		return buffer.toString();
	}
}