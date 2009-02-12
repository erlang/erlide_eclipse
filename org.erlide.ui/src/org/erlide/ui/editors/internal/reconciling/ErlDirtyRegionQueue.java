package org.erlide.ui.editors.internal.reconciling;

import java.util.ArrayList;
import java.util.List;

/**
 * Queue used by {@link org.eclipse.jface.text.reconciler.Reconciler} to manage
 * dirty regions. When a dirty region is inserted into the queue, the queue
 * tries to fold it into the neighboring dirty region.
 * 
 * @see org.eclipse.jface.text.reconciler.Reconciler
 * @see org.eclipse.jface.text.reconciler.DirtyRegion
 */
public class ErlDirtyRegionQueue {

	/** The list of dirty regions. */
	private final List<ErlDirtyRegion> fDirtyRegions = new ArrayList<ErlDirtyRegion>();

	/**
	 * Creates a new empty dirty region.
	 */
	public ErlDirtyRegionQueue() {
		super();
	}

	/**
	 * Adds a dirty region to the end of the dirty-region queue.
	 * 
	 * @param dr
	 *            the dirty region to add
	 */
	public void addDirtyRegion(final ErlDirtyRegion dr) {
		// If the dirty region being added is directly adjacent to the last
		// dirty region on the queue then merge the two dirty regions together.
		final ErlDirtyRegion lastDR = getLastDirtyRegion();
		if (lastDR != null && lastDR.isMergable(dr)) {
			lastDR.mergeWith(dr);
		} else {
			fDirtyRegions.add(dr);
		}
	}

	/**
	 * Returns the last dirty region that was added to the queue.
	 * 
	 * @return the last DirtyRegion on the queue
	 */
	private ErlDirtyRegion getLastDirtyRegion() {
		final int size = fDirtyRegions.size();
		return size == 0 ? null : fDirtyRegions.get(size - 1);
	}

	/**
	 * Returns the number of regions in the queue.
	 * 
	 * @return the dirty-region queue-size
	 */
	public int getSize() {
		return fDirtyRegions.size();
	}

	/**
	 * Throws away all entries in the queue.
	 */
	public void purgeQueue() {
		fDirtyRegions.clear();
	}

	/**
	 * Removes and returns the first dirty region in the queue
	 * 
	 * @return the next dirty region on the queue
	 */
	public ErlDirtyRegion removeNextDirtyRegion() {
		if (fDirtyRegions.size() == 0) {
			return null;
		}
		final ErlDirtyRegion dr = fDirtyRegions.get(0);
		fDirtyRegions.remove(0);
		return dr;
	}

	/**
	 * Removes and returns all dirty regions in the queue
	 * 
	 * @return all regions
	 */
	public List<ErlDirtyRegion> removeAllDirtyRegions() {
		if (fDirtyRegions.size() == 0) {
			return null;
		}
		final List<ErlDirtyRegion> d = new ArrayList<ErlDirtyRegion>(
				fDirtyRegions);
		fDirtyRegions.clear();
		return d;
	}

	public boolean isEmpty() {
		return fDirtyRegions.isEmpty();
	}
}
