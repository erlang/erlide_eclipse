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
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.util.ErlLogger;

public class ErlReconciler implements IReconciler {

    private final IErlReconcilingStrategy fStrategy;
    private final String path;
    ErlDirtyRegionQueue fDirtyRegionQueue;
    ReconcilerThread fThread;
    private Listener fListener;
    int fDelay = 500;
    boolean fIsIncrementalReconciler = true;
    IProgressMonitor fProgressMonitor;
    boolean fIsAllowedToModifyDocument = true;

    IDocument fDocument;
    private ITextViewer fViewer;
    /** True if it should reconcile all regions without delay between them */
    final boolean fChunkReconciler;

    private Object fMutex;

    public ErlReconciler(final IErlReconcilingStrategy strategy,
            final boolean isIncremental, final boolean chunkReconciler,
            final String path, final IErlModule module, final ITextEditor editor) {

        super();
        Assert.isNotNull(strategy);

        setIsIncrementalReconciler(isIncremental);
        fChunkReconciler = chunkReconciler;
        fStrategy = strategy;
        this.path = path;
        if (path != null) {
            ErlangEngine.getInstance().getModel().putEdited(path, module);
        }
        // https://bugs.eclipse.org/bugs/show_bug.cgi?id=63898
        if (editor instanceof ErlangEditor) {
            fMutex = ((ErlangEditor) editor).getReconcilerLock();
        } else {
            fMutex = new Object(); // Null Object
        }
    }

    /**
     * Background thread for the reconciling activity.
     */
    class ReconcilerThread extends Thread {

        private static final int RECONCILER_SUSPEND_LOOP_MAX = 10;
        private boolean fCanceled = false;
        private boolean fReset = false;
        private boolean fIsActive = false;

        public ReconcilerThread(final String name) {
            super(name);
            setPriority(Thread.MIN_PRIORITY);
            setDaemon(true);
        }

        public boolean isActive() {
            return fIsActive;
        }

        public boolean isDirty() {
            synchronized (fDirtyRegionQueue) {
                return !fDirtyRegionQueue.isEmpty();
            }
        }

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
                ErlLogger.debug("broke out of loop i %d isDirty %b", i, isDirty);
            }
        }

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
                        rs = fDirtyRegionQueue.extractAllDirtyRegions();
                    } else {
                        r = fDirtyRegionQueue.extractNextDirtyRegion();
                    }
                    fDirtyRegionQueue.notifyAll();
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
                fIsActive = false;
            }
        }
    }

    class Listener implements IDocumentListener, ITextInputListener {

        @Override
        public void documentAboutToBeChanged(final DocumentEvent e) {
        }

        @Override
        public void documentChanged(final DocumentEvent e) {
            // ErlLogger.debug("documentChanged %d %d %d", e.getOffset(),
            // e.getLength(), e.getText().length());
            if (!fThread.isDirty() && fThread.isAlive()) {
                if (!fIsAllowedToModifyDocument && Thread.currentThread() == fThread) {
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
                    && (fThread.isActive() || fThread.isDirty() && fThread.isAlive())) {
                fProgressMonitor.setCanceled(true);
            }

            if (fIsIncrementalReconciler) {
                createDirtyRegion(e);
            }

            fThread.reset();

        }

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
                        //                                fDocument.getLength(), ""); //$NON-NLS-1$
                        // createDirtyRegion(e);
                        fThread.reset();
                        fThread.suspendCallerWhileDirty();
                    }
                }

                fDocument = null;
            }
        }

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

    protected boolean isIncrementalReconciler() {
        return fIsIncrementalReconciler;
    }

    protected IDocument getDocument() {
        return fDocument;
    }

    protected ITextViewer getTextViewer() {
        return fViewer;
    }

    protected IProgressMonitor getProgressMonitor() {
        return fProgressMonitor;
    }

    @Override
    public void install(final ITextViewer textViewer) {

        Assert.isNotNull(textViewer);
        fViewer = textViewer;

        synchronized (this) {
            if (fThread != null) {
                return;
            }
            fThread = new ReconcilerThread(getClass().getName());
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
                final ReconcilerThread bt = fThread;
                fThread = null;
                bt.cancel();
            }
        }

        final ErlReconcilingStrategy s = (ErlReconcilingStrategy) getReconcilingStrategy(IDocument.DEFAULT_CONTENT_TYPE);
        s.uninstall();
        if (path != null) {
            ErlangEngine.getInstance().getModel().putEdited(path, null);
        }
    }

    protected void createDirtyRegion(final DocumentEvent e) {
        synchronized (fDirtyRegionQueue) {
            String text = e.getText();
            if (text == null) {
                text = "";
            }
            final ErlDirtyRegion erlDirtyRegion = new ErlDirtyRegion(e.getOffset(),
                    e.getLength(), text);
            fDirtyRegionQueue.addDirtyRegion(erlDirtyRegion);
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

    @Override
    public IReconcilingStrategy getReconcilingStrategy(final String contentType) {
        Assert.isNotNull(contentType);
        return fStrategy;
    }

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

    protected void reconcilerDocumentChanged(final IDocument document) {
        fStrategy.setDocument(document);
    }

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
        synchronized (fMutex) {
            if (fStrategy instanceof IReconcilingStrategyExtension) {
                final IReconcilingStrategyExtension extension = (IReconcilingStrategyExtension) fStrategy;
                extension.initialReconcile();
            }
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
                fDirtyRegionQueue.notifyAll();
            }
            fThread.reset();
            initialProcess();
        }
    }

}
