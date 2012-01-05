package org.erlide.core.internal.model.root;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IResourceDelta;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlElementDelta;
import org.erlide.core.model.root.IWorkingCopy;
import org.erlide.core.model.util.ElementChangedEvent;
import org.erlide.core.model.util.IElementChangedListener;

public class ErlModelDeltaManager {
    public static final int DEFAULT_CHANGE_EVENT = 0;

    private static final boolean verbose = false;
    /**
     * Turns delta firing on/off. By default it is on.
     */
    public boolean fFire;
    /**
     * Queue of reconcile deltas on working copies that have yet to be fired.
     * This is a table form IWorkingCopy to IErlElementDelta
     */
    public HashMap<IWorkingCopy, IErlElementDelta> reconcileDeltas;
    /**
     * Queue of deltas created explicitly by the model that have yet to be
     * fired.
     */
    public List<IErlElementDelta> erlModelDeltas;
    private final ErlModel model;

    public ErlModelDeltaManager(final ErlModel model) {
        this.model = model;
        fFire = true;
        reconcileDeltas = new HashMap<IWorkingCopy, IErlElementDelta>();
        erlModelDeltas = Collections
                .synchronizedList(new ArrayList<IErlElementDelta>());
    }

    /**
     * Fire Model deltas, flushing them after the fact. If the firing mode has
     * been turned off, this has no effect.
     */
    protected void fire(final IErlElementDelta customDeltas, final int eventType) {
        if (fFire) {
            IErlElementDelta deltaToNotify;
            if (customDeltas == null) {
                deltaToNotify = mergeDeltas(erlModelDeltas);
            } else {
                deltaToNotify = customDeltas;
            }

            final IElementChangedListener[] listeners;
            final int listenerCount;
            final int[] listenerMask;
            // Notification
            synchronized (model.elementChangedListeners) {
                listeners = new IElementChangedListener[model.elementChangedListeners
                        .size()];
                model.elementChangedListeners.toArray(listeners);
                listenerCount = listeners.length;
                listenerMask = null;
            }

            switch (eventType) {
            case DEFAULT_CHANGE_EVENT:
                // firePreAutoBuildDelta(deltaToNotify, listeners, listenerMask,
                // listenerCount);
                firePostChangeDelta(deltaToNotify, listeners, listenerMask,
                        listenerCount);
                fireReconcileDelta(listeners, listenerMask, listenerCount);
                break;
            // case ElementChangedEvent.PRE_AUTO_BUILD :
            // firePreAutoBuildDelta(deltaToNotify, listeners, listenerMask,
            // listenerCount);
            // break;
            case ElementChangedEvent.POST_CHANGE:
                firePostChangeDelta(deltaToNotify, listeners, listenerMask,
                        listenerCount);
                fireReconcileDelta(listeners, listenerMask, listenerCount);
                break;
            case ElementChangedEvent.POST_RECONCILE:
                fireReconcileDelta(listeners, listenerMask, listenerCount);
                break;
            case ElementChangedEvent.POST_SHIFT:
                fireShiftEvent(deltaToNotify, listeners, listenerMask,
                        listenerCount);
                return;
            }
        }
    }

    private void firePostChangeDelta(final IErlElementDelta deltaToNotify,
            final IElementChangedListener[] listeners,
            final int[] listenerMask, final int listenerCount) {

        // post change deltas
        if (verbose) {
            System.out
                    .println("FIRING POST_CHANGE Delta [" + Thread.currentThread() + "]:"); //$NON-NLS-1$//$NON-NLS-2$
            System.out
                    .println(deltaToNotify == null ? "<NONE>" : deltaToNotify.toString()); //$NON-NLS-1$
        }
        if (deltaToNotify != null) {
            // flush now so as to keep listener reactions to post their own
            // deltas for
            // subsequent iteration
            flushDeltas();
            model.notifyListeners(deltaToNotify,
                    ElementChangedEvent.POST_CHANGE, listeners, listenerMask,
                    listenerCount);
        }
    }

    private void fireReconcileDelta(final IElementChangedListener[] listeners,
            final int[] listenerMask, final int listenerCount) {
        final IErlElementDelta deltaToNotify = mergeDeltas(reconcileDeltas
                .values());
        if (verbose) {
            System.out
                    .println("FIRING POST_RECONCILE Delta [" + Thread.currentThread() + "]:"); //$NON-NLS-1$//$NON-NLS-2$
            System.out
                    .println(deltaToNotify == null ? "<NONE>" : deltaToNotify.toString()); //$NON-NLS-1$
        }
        if (deltaToNotify != null) {
            // flush now so as to keep listener reactions to post their own
            // deltas for
            // subsequent iteration
            reconcileDeltas.clear();
            model.notifyListeners(deltaToNotify,
                    ElementChangedEvent.POST_RECONCILE, listeners,
                    listenerMask, listenerCount);
        }
    }

    private void fireShiftEvent(final IErlElementDelta deltaToNotify,
            final IElementChangedListener[] listeners,
            final int[] listenerMask, final int listenerCount) {

        // post change deltas
        if (verbose) {
            System.out
                    .println("FIRING POST_SHIFT event [" + Thread.currentThread() + "]:"); //$NON-NLS-1$//$NON-NLS-2$
            System.out
                    .println(deltaToNotify == null ? "<NONE>" : deltaToNotify.toString()); //$NON-NLS-1$
        }
        if (deltaToNotify != null) {
            flushDeltas();
            model.notifyListeners(deltaToNotify,
                    ElementChangedEvent.POST_SHIFT, listeners, listenerMask,
                    listenerCount);
        }
    }

    /**
     * Flushes all deltas without firing them.
     */
    protected void flushDeltas() {
        erlModelDeltas.clear();
    }

    IErlElementDelta mergeDeltas(final Collection<IErlElementDelta> deltas) {

        synchronized (deltas) {
            if (deltas.size() == 0) {
                return null;
            }
            if (deltas.size() == 1) {
                return deltas.iterator().next();
            }
            if (deltas.size() <= 1) {
                return null;
            }

            final Iterator<IErlElementDelta> iterator = deltas.iterator();
            final IErlElement cRoot = ErlModelManager.getErlangModel();
            final ErlElementDelta rootDelta = new ErlElementDelta(0, 0, cRoot);
            boolean insertedTree = false;
            while (iterator.hasNext()) {
                final ErlElementDelta delta = (ErlElementDelta) iterator.next();
                final IErlElement element = delta.getElement();
                if (cRoot.equals(element)) {
                    final IErlElementDelta[] children = delta
                            .getChildren(IErlElementDelta.ALL);
                    for (final IErlElementDelta element0 : children) {
                        final ErlElementDelta projectDelta = (ErlElementDelta) element0;
                        rootDelta.insertDeltaTree(projectDelta.getElement(),
                                projectDelta);
                        insertedTree = true;
                    }
                    final IResourceDelta[] resourceDeltas = delta
                            .getResourceDeltas();
                    if (resourceDeltas != null) {
                        for (final IResourceDelta element0 : resourceDeltas) {
                            rootDelta.addResourceDelta(element0);
                            insertedTree = true;
                        }
                    }
                } else {
                    rootDelta.insertDeltaTree(element, delta);
                    insertedTree = true;
                }
            }
            if (insertedTree) {
                return rootDelta;
            }
            return null;
        }
    }
}
