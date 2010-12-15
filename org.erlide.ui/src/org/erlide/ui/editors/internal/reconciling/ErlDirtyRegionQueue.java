package org.erlide.ui.editors.internal.reconciling;

import java.util.ArrayList;
import java.util.List;

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
     * Throws away all entries in the queue.
     */
    public void purgeQueue() {
        fDirtyRegions.clear();
    }

    public ErlDirtyRegion getNextDirtyRegion() {
        if (fDirtyRegions.size() == 0) {
            return null;
        }
        final ErlDirtyRegion dr = fDirtyRegions.get(0);
        return dr;
    }

    public List<ErlDirtyRegion> getAllDirtyRegions() {
        if (fDirtyRegions.size() == 0) {
            return null;
        }
        final List<ErlDirtyRegion> d = new ArrayList<ErlDirtyRegion>(
                fDirtyRegions);
        return d;
    }

    public boolean isEmpty() {
        return fDirtyRegions.isEmpty();
    }

    public void removeAll(final List<ErlDirtyRegion> rs) {
        fDirtyRegions.removeAll(rs);
    }

    public void remove(final ErlDirtyRegion r) {
        fDirtyRegions.remove(r);
    }

}
