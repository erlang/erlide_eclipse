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

import java.text.NumberFormat;
import java.util.Enumeration;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.erlide.basiccore.ErlLogger;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IOpenable;
import org.erlide.core.erlang.internal.Openable;

/**
 * The buffer manager manages the set of open buffers. It implements an LRU
 * cache of buffers.
 */
public class BufferManager {

	protected static BufferManager defaultBufferManager;

	protected static boolean verbose;

	/**
	 * LRU cache of buffers. The key and value for an entry in the table is the
	 * identical buffer.
	 */
	protected OverflowingLRUCache openBuffers = new BufferCache(60);

	/**
	 * Adds a buffer to the table of open buffers.
	 */
	protected void addBuffer(IBuffer buffer) {
		if (verbose) {
			final String owner = ((Openable) buffer.getOwner())
					.toStringWithAncestors();
			ErlLogger.log("Adding buffer for " + owner); //$NON-NLS-1$
		}
		openBuffers.put(buffer.getOwner(), buffer);
		if (verbose) {
			System.out
					.println("-> Buffer cache filling ratio = " + NumberFormat.getInstance().format(openBuffers.fillingRatio()) + "%"); //$NON-NLS-1$//$NON-NLS-2$
		}
	}

	/**
	 * Creates a buffer for the given working copy. The new buffer will be
	 * initialized with the contents of the underlying file if and only if it
	 * was not already initialized by the compilation owner (a buffer is
	 * uninitialized if its content is <code>null</code>).
	 * <p>
	 * Note: This buffer will be associated to the working copy for its entire
	 * life-cycle. Another working copy on same unit but owned by a different
	 * owner would not share the same buffer unless its owner decided to
	 * implement such a sharing behaviour.
	 * </p>
	 * 
	 * @param workingCopy
	 *            the working copy of the buffer
	 * @return IBuffer the created buffer for the given working copy
	 * @see IBuffer
	 */
	public IBuffer createBuffer(IOpenable owner) {
		final IErlElement element = (IErlElement) owner;
		final IResource resource = element.getResource();
		final IBuffer buf = new Buffer(
				resource instanceof IFile ? (IFile) resource : null, owner,
				element.isReadOnly());
		addBuffer(buf);
		return buf;
	}

	/**
	 * Returns the open buffer associated with the given owner, or
	 * <code>null</code> if the owner does not have an open buffer associated
	 * with it.
	 */
	public IBuffer getBuffer(IOpenable owner) {
		return (IBuffer) openBuffers.get(owner);
	}

	/**
	 * Returns the default buffer manager.
	 */
	public static synchronized BufferManager getDefaultBufferManager() {
		if (defaultBufferManager == null) {
			defaultBufferManager = new BufferManager();
		}
		return defaultBufferManager;
	}

	/**
	 * Returns an enumeration of all open buffers.
	 * <p>
	 * The <code>Enumeration</code> answered is thread safe.
	 * 
	 * @see OverflowingLRUCache
	 * @return Enumeration of IBuffer
	 */
	@SuppressWarnings("unchecked")
	public Enumeration getOpenBuffers() {
		synchronized (openBuffers) {
			openBuffers.shrink();
			return openBuffers.elements();
		}
	}

	/**
	 * Removes a buffer from the table of open buffers.
	 */
	public void removeBuffer(IBuffer buffer) {
		if (verbose) {
			final String owner = ((Openable) buffer.getOwner())
					.toStringWithAncestors();
			ErlLogger.log("Removing buffer for " + owner); //$NON-NLS-1$
		}
		openBuffers.remove(buffer.getOwner());
		if (verbose) {
			System.out
					.println("-> Buffer cache filling ratio = " + NumberFormat.getInstance().format(openBuffers.fillingRatio()) + "%"); //$NON-NLS-1$//$NON-NLS-2$
		}
	}
}
