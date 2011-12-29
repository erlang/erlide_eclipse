package org.erlide.ui.editors.internal.reconciling;

import java.util.List;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.jface.text.ITextInputListener;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.reconciler.AbstractReconciler;
import org.eclipse.jface.text.reconciler.DirtyRegion;
import org.eclipse.jface.text.reconciler.IReconciler;
import org.eclipse.jface.text.reconciler.IReconcilingStrategy;
import org.eclipse.jface.text.reconciler.IReconcilingStrategyExtension;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.jinterface.ErlLogger;

public class ErlReconciler implements IReconciler {

    /** The reconciling strategy. */
    private final IErlReconcilingStrategy fStrategy;
    private final String path;
    /** Queue to manage the changes applied to the text viewer. */
    ErlDirtyRegionQueue fDirtyRegionQueue;
    /** The background thread. */
    BackgroundThread fThread;
    /** Internal document and text input listener. */
    private Listener fListener;
    /** The background thread delay. */
    int fDelay = 500;
    /** Are there incremental reconciling strategies? */
    boolean fIsIncrementalReconciler = true;
    /** The progress monitor used by this reconciler. */
    IProgressMonitor fProgressMonitor;
    /** Tells whether this reconciler is allowed to modify the document. */
    boolean fIsAllowedToModifyDocument = true;

    /** The text viewer's document. */
    IDocument fDocument;
    /** The text viewer */
    private ITextViewer fViewer;
    /** True if it should reconcile all regions without delay between them */
    final boolean fChunkReconciler;

    public ErlReconciler(final IErlReconcilingStrategy strategy,
            final boolean isIncremental, final boolean chunkReconciler,
            final String path, final IErlModule module) {

        super();
        Assert.isNotNull(strategy);

        setIsIncrementalReconciler(isIncremental);
        fChunkReconciler = chunkReconciler;
        fStrategy = strategy;
        this.path = path;
        if (path != null) {
            ErlModelManager.getErlangModel().putEdited(path, module);
        }
    }

    /**
     * Background thread for the reconciling activity.
     */
    class BackgroundThread extends Thread {

        private static final int RECONCILER_SUSPEND_LOOP_MAX = 10;
        /** Has the reconciler been canceled. */
        private boolean fCanceled = false;
        /** Has the reconciler been reset. */
        private boolean fReset = false;
        /** Is a reconciling strategy active. */
        private boolean fIsActive = false;

        /**
         * Creates a new background thread. The thread runs with minimal
         * priority.
         * 
         * @param name
         *            the thread's name
         */
        public BackgroundThread(final String name) {
            super(name);
            setPriority(Thread.MIN_PRIORITY);
            setDaemon(true);
        }

        /**
         * Returns whether a reconciling strategy is active right now.
         * 
         * @return <code>true</code> if a activity is active
         */
        public boolean isActive() {
            return fIsActive;
        }

        /**
         * Returns whether some changes need to be processed.
         * 
         * @return <code>true</code> if changes wait to be processed
         * @since 3.0
         */
        public boolean isDirty() {
            synchronized (fDirtyRegionQueue) {
                return !fDirtyRegionQueue.isEmpty();
            }
        }

        /**
         * Cancels the background thread.
         */
        public void cancel() {
            fCanceled = true;
            final IProgressMonitor pm = fProgressMonitor;
            if (pm != null) {
                pm.setCanceled(true);
            }
            synchronized (fDirtyRegionQueue) {
                fDirtyRegionQueue.notifyAll();
            }
        }

        /**
         * Suspends the caller of this method until this background thread has
         * emptied the dirty region queue.
         */
        public void suspendCallerWhileDirty() {
            boolean isDirty = true;
            int i = RECONCILER_SUSPEND_LOOP_MAX;
            while (i > 0 && isDirty) {
                i--;
                synchronized (fDirtyRegionQueue) {
                    isDirty = isDirty();
                    if (isDirty) {
                        try {
                            fDirtyRegionQueue.wait(fDelay);
                        } catch (final InterruptedException x) {
                        }
                    }
                }
            }
            if (i == 0 || isDirty) {
                ErlLogger
                        .debug("broke out of loop i %d isDirty %b", i, isDirty);
            }
        }

        /**
         * Reset the background thread as the text viewer has been changed,
         */
        public synchronized void reset() {
            if (fDelay > 0) {
                fReset = true;
            } else {
                synchronized (fDirtyRegionQueue) {
                    fDirtyRegionQueue.notifyAll();
                }
            }
            reconcilerReset();
        }

        /**
         * Set reset flag to false, so that it will reconcile, only to be used
         * by {@link ErlReconciler#reconcileNow()}
         */
        public synchronized void unreset() {
            fReset = false;
        }

        /**
         * The background activity. Waits until there is something in the queue
         * managing the changes that have been applied to the text viewer.
         * Removes the first change from the queue and process it. If
         * fReconcileAllAtOnce is set, it removes all changes and processes
         * them.
         * <p>
         * Calls {@link AbstractReconciler#initialProcess()} on entrance.
         * </p>
         */
        @Override
        public void run() {

            synchronized (fDirtyRegionQueue) {
                try {
                    fDirtyRegionQueue.wait(fDelay);
                } catch (final InterruptedException x) {
                }
            }

            initialProcess();

            while (!fCanceled) {
                synchronized (fDirtyRegionQueue) {
                    try {
                        fDirtyRegionQueue.wait(fDelay);
                    } catch (final InterruptedException x) {
                    }
                }

                if (fCanceled) {
                    break;
                }
                synchronized (fDirtyRegionQueue) {
                    if (fDirtyRegionQueue.isEmpty()) {
                        continue;
                    }
                }
                synchronized (this) {
                    if (fReset) {
                        fReset = false;
                        continue;
                    }
                }

                List<ErlDirtyRegion> rs = null;
                ErlDirtyRegion r = null;
                synchronized (fDirtyRegionQueue) {
                    if (fChunkReconciler) {
                        rs = fDirtyRegionQueue.getAllDirtyRegions();
                    } else {
                        r = fDirtyRegionQueue.getNextDirtyRegion();
                    }
                }
                fIsActive = true;

                if (fProgressMonitor != null) {
                    fProgressMonitor.setCanceled(false);
                }

                if (fChunkReconciler) {
                    if (rs != null) {
                        for (final ErlDirtyRegion dirtyRegion : rs) {
                            process(dirtyRegion);
                        }
                    }
                } else {
                    process(r);
                }
                postProcess();
                synchronized (fDirtyRegionQueue) {
                    if (fChunkReconciler) {
                        fDirtyRegionQueue.removeAll(rs);
                    } else {
                        fDirtyRegionQueue.remove(r);
                    }
                    fDirtyRegionQueue.notifyAll();
                }
                fIsActive = false;
            }
        }
    }

    /**
     * Internal document listener and text input listener.
     */
    class Listener implements IDocumentListener, ITextInputListener {

        /*
         * @see IDocumentListener#documentAboutToBeChanged(DocumentEvent)
         */
        @Override
        public void documentAboutToBeChanged(final DocumentEvent e) {
        }

        /*
         * @see IDocumentListener#documentChanged(DocumentEvent)
         */
        @Override
        public void documentChanged(final DocumentEvent e) {
            if (!fThread.isDirty() && fThread.isAlive()) {
                if (!fIsAllowedToModifyDocument
                        && Thread.currentThread() == fThread) {
                    throw new UnsupportedOperationException(
                            "The reconciler thread is not allowed to modify the document"); //$NON-NLS-1$
                }
                aboutToBeReconciled();
            }

            /*
             * The second OR condition handles the case when the document gets
             * changed while still inside initialProcess().
             */
            if (fProgressMonitor != null
                    && (fThread.isActive() || fThread.isDirty()
                            && fThread.isAlive())) {
                fProgressMonitor.setCanceled(true);
            }

            if (fIsIncrementalReconciler) {
                createDirtyRegion(e);
            }

            fThread.reset();

        }

        /*
         * @see ITextInputListener#inputDocumentAboutToBeChanged(IDocument,
         * IDocument)
         */
        @Override
        public void inputDocumentAboutToBeChanged(final IDocument oldInput,
                final IDocument newInput) {

            if (oldInput == fDocument) {

                if (fDocument != null) {
                    fDocument.removeDocumentListener(this);
                }

                if (fIsIncrementalReconciler) {
                    synchronized (fDirtyRegionQueue) {
                        fDirtyRegionQueue.purgeQueue();
                    }
                    if (fDocument != null && fDocument.getLength() > 0) {
                        // final DocumentEvent e = new DocumentEvent(fDocument,
                        // 0,
                        //								fDocument.getLength(), ""); //$NON-NLS-1$
                        // createDirtyRegion(e);
                        fThread.reset();
                        fThread.suspendCallerWhileDirty();
                    }
                }

                fDocument = null;
            }
        }

        /*
         * @see ITextInputListener#inputDocumentChanged(IDocument, IDocument)
         */
        @Override
        public void inputDocumentChanged(final IDocument oldInput,
                final IDocument newInput) {

            fDocument = newInput;
            if (fDocument == null) {
                return;
            }

            reconcilerDocumentChanged(fDocument);

            fDocument.addDocumentListener(this);

            if (!fThread.isDirty()) {
                aboutToBeReconciled();
            }

            // if (fIsIncrementalReconciler) {
            // final DocumentEvent e = new DocumentEvent(fDocument, 0, 0,
            // fDocument.get());
            // createDirtyRegion(e);
            // }

            startReconciling();
        }
    }

    /**
     * Tells the reconciler how long it should wait for further text changes
     * before activating the appropriate reconciling strategies.
     * 
     * @param delay
     *            the duration in milliseconds of a change collection period.
     */
    public void setDelay(final int delay) {
        fDelay = delay;
    }

    /**
     * Tells the reconciler whether any of the available reconciling strategies
     * is interested in getting detailed dirty region information or just in the
     * fact that the document has been changed. In the second case, the
     * reconciling can not incrementally be pursued.
     * 
     * @param isIncremental
     *            indicates whether this reconciler will be configured with
     *            incremental reconciling strategies
     * 
     * @see DirtyRegion
     * @see IReconcilingStrategy
     */
    public void setIsIncrementalReconciler(final boolean isIncremental) {
        fIsIncrementalReconciler = isIncremental;
    }

    /**
     * Tells the reconciler whether it is allowed to change the document inside
     * its reconciler thread.
     * <p>
     * If this is set to <code>false</code> an
     * {@link UnsupportedOperationException} will be thrown when this
     * restriction will be violated.
     * </p>
     * 
     * @param isAllowedToModify
     *            indicates whether this reconciler is allowed to modify the
     *            document
     * @since 3.2
     */
    public void setIsAllowedToModifyDocument(final boolean isAllowedToModify) {
        fIsAllowedToModifyDocument = isAllowedToModify;
    }

    /**
     * Returns whether any of the reconciling strategies is interested in
     * detailed dirty region information.
     * 
     * @return whether this reconciler is incremental
     * 
     * @see IReconcilingStrategy
     */
    protected boolean isIncrementalReconciler() {
        return fIsIncrementalReconciler;
    }

    /**
     * Returns the input document of the text viewer this reconciler is
     * installed on.
     * 
     * @return the reconciler document
     */
    protected IDocument getDocument() {
        return fDocument;
    }

    /**
     * Returns the text viewer this reconciler is installed on.
     * 
     * @return the text viewer this reconciler is installed on
     */
    protected ITextViewer getTextViewer() {
        return fViewer;
    }

    /**
     * Returns the progress monitor of this reconciler.
     * 
     * @return the progress monitor of this reconciler
     */
    protected IProgressMonitor getProgressMonitor() {
        return fProgressMonitor;
    }

    /*
     * @see IReconciler#install(ITextViewer)
     */
    @Override
    public void install(final ITextViewer textViewer) {

        Assert.isNotNull(textViewer);
        fViewer = textViewer;

        synchronized (this) {
            if (fThread != null) {
                return;
            }
            fThread = new BackgroundThread(getClass().getName());
        }

        fDirtyRegionQueue = new ErlDirtyRegionQueue();

        fListener = new Listener();
        fViewer.addTextInputListener(fListener);

        // see bug https://bugs.eclipse.org/bugs/show_bug.cgi?id=67046
        // if the reconciler gets installed on a viewer that already has a
        // document
        // (e.g. when reusing editors), we force the listener to register
        // itself as document listener, because there will be no input change
        // on the viewer.
        // In order to do that, we simulate an input change.
        final IDocument document = textViewer.getDocument();
        if (document != null) {
            fListener.inputDocumentAboutToBeChanged(fDocument, document);
            fListener.inputDocumentChanged(fDocument, document);
        }
    }

    /*
     * @see IReconciler#uninstall()
     */
    @Override
    public void uninstall() {
        if (fListener != null) {

            fViewer.removeTextInputListener(fListener);
            if (fDocument != null) {
                fListener.inputDocumentAboutToBeChanged(fDocument, null);
                fListener.inputDocumentChanged(fDocument, null);
            }
            fListener = null;

            synchronized (this) {
                // http://dev.eclipse.org/bugs/show_bug.cgi?id=19135
                final BackgroundThread bt = fThread;
                fThread = null;
                bt.cancel();
            }
        }

        final ErlReconcilerStrategy s = (ErlReconcilerStrategy) getReconcilingStrategy(IDocument.DEFAULT_CONTENT_TYPE);
        s.uninstall();
        if (path != null) {
            ErlModelManager.getErlangModel().putEdited(path, null);
        }
    }

    /**
     * Creates a dirty region for a document event and adds it to the queue.
     * 
     * @param e
     *            the document event for which to create a dirty region
     */
    protected void createDirtyRegion(final DocumentEvent e) {
        synchronized (fDirtyRegionQueue) {
            String text = e.getText();
            if (text == null) {
                text = "";
            }
            fDirtyRegionQueue.addDirtyRegion(new ErlDirtyRegion(e.getOffset(),
                    e.getLength(), text));
            fDirtyRegionQueue.notifyAll();
        }
    }

    /**
     * Hook for subclasses which want to perform some action as soon as
     * reconciliation is needed.
     * <p>
     * Default implementation is to do nothing.
     * </p>
     * 
     * @since 3.0
     */
    protected void aboutToBeReconciled() {
    }

    /**
     * Forces the reconciler to reconcile the structure of the whole document.
     * Clients may extend this method.
     */
    public void forceReconciling() {

        if (fDocument != null) {

            if (!fThread.isDirty() && fThread.isAlive()) {
                aboutToBeReconciled();
            }

            if (fProgressMonitor != null && fThread.isActive()) {
                fProgressMonitor.setCanceled(true);
            }

            if (fIsIncrementalReconciler) {
                final DocumentEvent e = new DocumentEvent(fDocument, 0,
                        fDocument.getLength(), fDocument.get());
                createDirtyRegion(e);
            }

            startReconciling();
        }
    }

    /**
     * Starts the reconciler to reconcile the queued dirty-regions. Clients may
     * extend this method.
     */
    protected synchronized void startReconciling() {
        if (fThread == null) {
            return;
        }

        if (!fThread.isAlive()) {
            try {
                fThread.start();
            } catch (final IllegalThreadStateException e) {
                // see https://bugs.eclipse.org/bugs/show_bug.cgi?id=40549
                // This is the only instance where the thread is started; since
                // we checked that it is not alive, it must be dead already due
                // to a run-time exception or error. Exit.
            }
        } else {
            fThread.reset();
        }
    }

    /**
     * Hook that is called after the reconciler thread has been reset.
     */
    protected void reconcilerReset() {
    }

    /*
     * @see IReconciler#getReconcilingStrategy(String)
     */
    @Override
    public IReconcilingStrategy getReconcilingStrategy(final String contentType) {
        Assert.isNotNull(contentType);
        return fStrategy;
    }

    /*
     * @see AbstractReconciler#process(DirtyRegion)
     */
    protected void process(final ErlDirtyRegion dirtyRegion) {
        if (dirtyRegion != null) {
            fStrategy.reconcile(dirtyRegion);
        } else {
            final IDocument document = getDocument();
            if (document != null) {
                fStrategy.reconcile(new Region(0, document.getLength()));
            }
        }
    }

    protected void postProcess() {
        fStrategy.chunkReconciled();
    }

    /*
     * @see AbstractReconciler#reconcilerDocumentChanged(IDocument)
     */
    protected void reconcilerDocumentChanged(final IDocument document) {
        fStrategy.setDocument(document);
    }

    /*
     * @see AbstractReconciler#setProgressMonitor(IProgressMonitor)
     */
    public void setProgressMonitor(final IProgressMonitor monitor) {
        fProgressMonitor = monitor;
        if (fStrategy instanceof IReconcilingStrategyExtension) {
            final IReconcilingStrategyExtension extension = (IReconcilingStrategyExtension) fStrategy;
            extension.setProgressMonitor(monitor);
        }
    }

    /**
     * This method is called on startup of the background activity. It is called
     * only once during the life time of the reconciler.
     */
    protected void initialProcess() {
        if (fStrategy instanceof IReconcilingStrategyExtension) {
            final IReconcilingStrategyExtension extension = (IReconcilingStrategyExtension) fStrategy;
            extension.initialReconcile();
        }
    }

    public void reconcileNow() {
        fThread.unreset();
        synchronized (fDirtyRegionQueue) {
            fDirtyRegionQueue.notifyAll();
        }
        fThread.suspendCallerWhileDirty();
    }

    public void reset() {
        if (fIsIncrementalReconciler) {
            synchronized (fDirtyRegionQueue) {
                fDirtyRegionQueue.purgeQueue();
            }
            fThread.reset();
            initialProcess();
        }
    }

}
