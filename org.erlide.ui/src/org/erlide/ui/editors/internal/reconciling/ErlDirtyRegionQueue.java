package org.erlide.ui.editors.internal.reconciling;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import com.google.common.collect.Lists;

/**
 * Queue used by {@link org.eclipse.jface.text.reconciler.Reconciler} to manage
 * dirty regions. When a dirty region is inserted into the queue, the queue
 * tries to fold it into the neighbouring dirty region.
 *
 * @see org.eclipse.jface.text.reconciler.Reconciler
 * @see org.eclipse.jface.text.reconciler.DirtyRegion
 */
public class ErlDirtyRegionQueue {

    /** The list of dirty regions. */
    private final LinkedList<ErlDirtyRegion> fDirtyRegions = Lists.newLinkedList();

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
     * @return true if the region is added, false if it's merged to previous
     *
     */
    public boolean addDirtyRegion(final ErlDirtyRegion dr) {
        // If the dirty region being added is directly adjacent to the last
        // dirty region on the queue then merge the two dirty regions together.
        final ErlDirtyRegion lastDR = fDirtyRegions.peekLast();
        if (lastDR != null && lastDR.isMergable(dr)) {
            lastDR.mergeWith(dr);
            return false;
        }
        fDirtyRegions.addLast(dr);
        return true;
    }

    /**
     * Throws away all entries in the queue.
     */
    public void purgeQueue() {
        fDirtyRegions.clear();
    }

    public ErlDirtyRegion extractNextDirtyRegion() {
        return fDirtyRegions.pollFirst();
    }

    public List<ErlDirtyRegion> extractAllDirtyRegions() {
        final List<ErlDirtyRegion> d = new ArrayList<ErlDirtyRegion>(fDirtyRegions);
        fDirtyRegions.clear();
        return d;
    }

    public boolean isEmpty() {
        return fDirtyRegions.isEmpty();
    }

}
