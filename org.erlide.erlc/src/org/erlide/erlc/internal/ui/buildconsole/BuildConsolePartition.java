/*******************************************************************************
 * Copyright (c) 2002, 2004 QNX Software Systems and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * QNX Software Systems - Initial API and implementation
 *******************************************************************************/
package org.erlide.erlc.internal.ui.buildconsole;

import org.eclipse.jface.text.TypedRegion;
import org.erlide.erlc.ErlideErlcPlugin;

public class BuildConsolePartition extends TypedRegion {

	/**
	 * Associated stream
	 */
	private BuildConsoleStream fStream;

	/**
	 * Partition type
	 */
	public static final String CONSOLE_PARTITION_TYPE = ErlideErlcPlugin.PLUGIN_ID
			+ ".CONSOLE_PARTITION_TYPE"; //$NON-NLS-1$	

	public BuildConsolePartition(BuildConsoleStream stream, int offset,
			int length) {
		super(offset, length, CONSOLE_PARTITION_TYPE);
		fStream = stream;
	}

	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object partition) {
		if (super.equals(partition)) {
			return fStream.equals(((BuildConsolePartition) partition)
					.getStream());
		}
		return false;
	}

	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return super.hashCode() + fStream.hashCode();
	}

	/**
	 * Returns this partition's stream
	 * 
	 * @return this partition's stream
	 */
	public BuildConsoleStream getStream() {
		return fStream;
	}

	/**
	 * Returns whether this partition is allowed to be combined with the given
	 * partition.
	 * 
	 * @param partition
	 * @return boolean
	 */
	public boolean canBeCombinedWith(BuildConsolePartition partition) {
		final int start = getOffset();
		final int end = start + getLength();
		final int otherStart = partition.getOffset();
		final int otherEnd = otherStart + partition.getLength();
		final boolean overlap = (otherStart >= start && otherStart <= end)
				|| (start >= otherStart && start <= otherEnd);
		return overlap && getType().equals(partition.getType())
				&& getStream().equals(partition.getStream());
	}

	/**
	 * Returns a new partition representing this and the given parition
	 * combined.
	 * 
	 * @param partition
	 * @return partition
	 */
	public BuildConsolePartition combineWith(BuildConsolePartition partition) {
		final int start = getOffset();
		final int end = start + getLength();
		final int otherStart = partition.getOffset();
		final int otherEnd = otherStart + partition.getLength();
		final int theStart = Math.min(start, otherStart);
		final int theEnd = Math.max(end, otherEnd);
		return createNewPartition(theStart, theEnd - theStart);
	}

	/**
	 * Creates a new patition of this type with the given color, offset, and
	 * length.
	 * 
	 * @param offset
	 * @param length
	 * @return a new partition with the given range
	 */
	public BuildConsolePartition createNewPartition(int offset, int length) {
		return new BuildConsolePartition(getStream(), offset, length);
	}
}
