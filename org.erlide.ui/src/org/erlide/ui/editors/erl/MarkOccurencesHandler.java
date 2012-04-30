package org.erlide.ui.editors.erl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentExtension4;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ISelectionValidator;
import org.eclipse.jface.text.ITextInputListener;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.link.LinkedModeModel;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.IAnnotationModelExtension;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.viewers.IPostSelectionProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.erlide.backend.BackendCore;
import org.erlide.backend.IBackend;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.util.ModelUtils;
import org.erlide.core.services.search.ErlSearchScope;
import org.erlide.core.services.search.ErlangSearchPattern;
import org.erlide.core.services.search.ErlangSearchPattern.LimitTo;
import org.erlide.core.services.search.ErlideOpen;
import org.erlide.core.services.search.ErlideSearchServer;
import org.erlide.core.services.search.ModuleLineFunctionArityRef;
import org.erlide.core.services.search.OpenResult;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.jinterface.rpc.RpcTimeoutException;
import org.erlide.ui.editors.erl.ErlangEditor.ActivationListener;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.internal.search.ErlangSearchElement;
import org.erlide.ui.internal.search.SearchUtil;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.google.common.collect.Lists;

public class MarkOccurencesHandler {

    private final ErlangEditor erlangEditor;

    /**
     * Finds and marks occurrence annotations.
     * 
     * @since 3.0
     */
    class OccurrencesFinderJob extends Job {

        private final IDocument fDocument;
        private final ITextSelection selection;
        private final ISelectionValidator fPostSelectionValidator;
        private boolean fCanceled = false;
        private List<MarkOccurencesHandler.ErlangRef> fRefs;
        private final boolean fHasChanged;
        private final IErlModule module;

        public OccurrencesFinderJob(final IDocument document,
                final IErlModule module, final ITextSelection selection,
                final boolean hasChanged) {
            super("OccurrencesFinderJob");
            fDocument = document;
            this.selection = selection;

            if (erlangEditor.getSelectionProvider() instanceof ISelectionValidator) {
                fPostSelectionValidator = (ISelectionValidator) erlangEditor
                        .getSelectionProvider();
            } else {
                fPostSelectionValidator = null;
            }
            this.module = module;
            fHasChanged = hasChanged;
        }

        private void findRefs(final IErlModule theModule,
                final ITextSelection aSelection, final boolean hasChanged) {
            final IBackend ideBackend = BackendCore.getBackendManager()
                    .getIdeBackend();
            fRefs = null;

            if (fCanceled) {
                return;
            }
            try {
                final int offset = aSelection.getOffset();
                final OpenResult res = ErlideOpen.open(ideBackend, theModule,
                        offset, ModelUtils.getImportsAsList(theModule), "",
                        ErlModelManager.getErlangModel().getPathVars());
                final ErlangSearchPattern pattern = SearchUtil
                        .getSearchPatternFromOpenResultAndLimitTo(theModule,
                                offset, res, LimitTo.ALL_OCCURRENCES, false);
                if (fCanceled) {
                    return;
                }
                if (pattern != null) {
                    final ErlSearchScope scope = new ErlSearchScope();
                    scope.addModule(theModule);
                    final List<ModuleLineFunctionArityRef> findRefs = Lists
                            .newArrayList();
                    final OtpErlangObject refs = ErlideSearchServer.findRefs(
                            ideBackend, pattern, scope,
                            erlangEditor.getStateDir());
                    if (refs != null) {
                        SearchUtil.addSearchResult(findRefs, refs);
                        fRefs = erlangEditor.markOccurencesHandler
                                .getErlangRefs(theModule, findRefs);
                    }
                }
            } catch (final RpcTimeoutException e) {
                if (!ideBackend.isStopped()) {
                    ErlLogger.warn(e);
                }
            } catch (final RpcException e) {
                ErlLogger.debug(e);
            } catch (final ErlModelException e) {
                ErlLogger.debug(e);
            } catch (final OtpErlangRangeException e) {
                ErlLogger.debug(e);
            }
            if (fRefs == null) {
                if (!erlangEditor.markOccurencesHandler.fStickyOccurrenceAnnotations) {
                    erlangEditor.markOccurencesHandler
                            .removeOccurrenceAnnotations();
                } else if (hasChanged) {
                    erlangEditor.markOccurencesHandler
                            .removeOccurrenceAnnotations();
                }
            }
        }

        // cannot use cancel() because it is declared final
        void doCancel() {
            fCanceled = true;
            cancel();
        }

        private boolean isCanceled(final IProgressMonitor progressMonitor) {
            return fCanceled
                    || progressMonitor.isCanceled()
                    || fPostSelectionValidator != null
                    && !(fPostSelectionValidator.isValid(selection) || erlangEditor.markOccurencesHandler.fForcedMarkOccurrencesSelection == selection)
                    || LinkedModeModel.hasInstalledModel(fDocument);
        }

        /*
         * @see Job#run(org.eclipse.core.runtime.IProgressMonitor)
         */
        @Override
        public IStatus run(final IProgressMonitor progressMonitor) {
            findRefs(module, selection, fHasChanged);
            if (fRefs == null) {
                return Status.CANCEL_STATUS;
            }

            if (isCanceled(progressMonitor)) {
                return Status.CANCEL_STATUS;
            }

            final ITextViewer textViewer = erlangEditor.getViewer();
            if (textViewer == null) {
                return Status.CANCEL_STATUS;
            }

            final IDocument document = textViewer.getDocument();
            if (document == null) {
                return Status.CANCEL_STATUS;
            }

            final IDocumentProvider documentProvider = erlangEditor
                    .getDocumentProvider();
            if (documentProvider == null) {
                return Status.CANCEL_STATUS;
            }

            final IAnnotationModel annotationModel = documentProvider
                    .getAnnotationModel(erlangEditor.getEditorInput());
            if (annotationModel == null) {
                return Status.CANCEL_STATUS;
            }

            // Add occurrence annotations
            final HashMap<Annotation, Position> annotationMap = new HashMap<Annotation, Position>(
                    fRefs.size());
            for (final MarkOccurencesHandler.ErlangRef ref : fRefs) {
                if (isCanceled(progressMonitor)) {
                    return Status.CANCEL_STATUS;
                }

                final Position position = new Position(ref.getOffset(),
                        ref.getLength());

                final String description = ref.getDescription();
                final String annotationType = ref.isDef() ? "org.erlide.ui.occurrences.definition" //$NON-NLS-1$
                        : "org.erlide.ui.occurrences";

                annotationMap.put(new Annotation(annotationType, false,
                        description), position);
            }

            if (isCanceled(progressMonitor)) {
                return Status.CANCEL_STATUS;
            }

            synchronized (erlangEditor.getLockObject(annotationModel)) {
                if (annotationModel instanceof IAnnotationModelExtension) {
                    ((IAnnotationModelExtension) annotationModel)
                            .replaceAnnotations(
                                    erlangEditor.markOccurencesHandler.fOccurrenceAnnotations,
                                    annotationMap);
                } else {
                    erlangEditor.markOccurencesHandler
                            .removeOccurrenceAnnotations();
                    for (final Map.Entry<Annotation, Position> mapEntry : annotationMap
                            .entrySet()) {
                        annotationModel.addAnnotation(mapEntry.getKey(),
                                mapEntry.getValue());
                    }
                }
                erlangEditor.markOccurencesHandler.fOccurrenceAnnotations = annotationMap
                        .keySet().toArray(
                                new Annotation[annotationMap.keySet().size()]);
            }

            return Status.OK_STATUS;
        }
    }

    /**
     * Cancels the occurrences finder job upon document changes.
     * 
     * @since 3.0
     */
    class OccurrencesFinderJobCanceler implements IDocumentListener,
            ITextInputListener {

        public void install() {
            final ISourceViewer sourceViewer = erlangEditor.getViewer();
            if (sourceViewer == null) {
                return;
            }

            final StyledText text = sourceViewer.getTextWidget();
            if (text == null || text.isDisposed()) {
                return;
            }

            sourceViewer.addTextInputListener(this);

            final IDocument document = sourceViewer.getDocument();
            if (document != null) {
                document.addDocumentListener(this);
            }
        }

        public void uninstall() {
            final ISourceViewer sourceViewer = erlangEditor.getViewer();
            if (sourceViewer != null) {
                sourceViewer.removeTextInputListener(this);
            }

            final IDocumentProvider documentProvider = erlangEditor
                    .getDocumentProvider();
            if (documentProvider != null) {
                final IDocument document = documentProvider
                        .getDocument(erlangEditor.getEditorInput());
                if (document != null) {
                    document.removeDocumentListener(this);
                }
            }
        }

        /*
         * @see
         * org.eclipse.jface.text.IDocumentListener#documentAboutToBeChanged
         * (org.eclipse.jface.text.DocumentEvent)
         */
        @Override
        public void documentAboutToBeChanged(final DocumentEvent event) {
            if (erlangEditor.markOccurencesHandler.fOccurrencesFinderJob != null) {
                erlangEditor.markOccurencesHandler.fOccurrencesFinderJob
                        .doCancel();
            }
        }

        /*
         * @see
         * org.eclipse.jface.text.IDocumentListener#documentChanged(org.eclipse
         * .jface.text.DocumentEvent)
         */
        @Override
        public void documentChanged(final DocumentEvent event) {
        }

        /*
         * @see
         * org.eclipse.jface.text.ITextInputListener#inputDocumentAboutToBeChanged
         * (org.eclipse.jface.text.IDocument, org.eclipse.jface.text.IDocument)
         */
        @Override
        public void inputDocumentAboutToBeChanged(final IDocument oldInput,
                final IDocument newInput) {
            if (oldInput == null) {
                return;
            }

            oldInput.removeDocumentListener(this);
        }

        /*
         * @see
         * org.eclipse.jface.text.ITextInputListener#inputDocumentChanged(org
         * .eclipse.jface.text.IDocument, org.eclipse.jface.text.IDocument)
         */
        @Override
        public void inputDocumentChanged(final IDocument oldInput,
                final IDocument newInput) {
            if (newInput == null) {
                return;
            }
            newInput.addDocumentListener(this);
        }
    }

    /**
     * Private class used by mark occurrences
     * 
     * @author jakob
     * 
     */
    static class ErlangRef {
        final private ErlangSearchElement element;
        final private int offset;
        final private int length;
        final private boolean def;

        public ErlangRef(final ErlangSearchElement element, final int offset,
                final int length, final boolean def) {
            super();
            this.element = element;
            this.offset = offset;
            this.length = length;
            this.def = def;
        }

        // public ErlangSearchElement getElement() {
        // return element;
        // }

        public int getOffset() {
            return offset;
        }

        public int getLength() {
            return length;
        }

        // public IErlElement.Kind getKind() {
        // return element.getKind();
        // }

        public boolean isDef() {
            return def;
        }

        public String getDescription() {
            return element.toString();
        }

    }

    /**
     * Tells whether the occurrence annotations are sticky i.e. whether they
     * stay even if there's no valid erlang element at the current caret
     * position. Only valid if {@link #markOccurencesHandler.fMarkOccurrenceAnnotations} is
     * <code>true</code>.
     * 
     * @since 3.0
     */
    public boolean fStickyOccurrenceAnnotations;
    /**
     * Holds the current occurrence annotations.
     * 
     * @since 3.0
     */
    public Annotation[] fOccurrenceAnnotations;
    /**
     * Tells whether all occurrences of the element at the current caret
     * location are automatically marked in this editor.
     * 
     * @since 3.0
     */
    public boolean fMarkOccurrenceAnnotations;
    /**
     * The selection used when forcing occurrence marking through code.
     * 
     * @since 3.0
     */
    public ISelection fForcedMarkOccurrencesSelection;
    /**
     * The document modification stamp at the time when the last occurrence
     * marking took place.
     * 
     * @since 3.1
     */
    public long fMarkOccurrenceModificationStamp;
    /**
     * The region of the word under the caret used to when computing the current
     * occurrence markings.
     * 
     * @since 3.1
     */
    public IRegion fMarkOccurrenceTargetRegion;
    /**
     * The internal shell activation listener for updating occurrences.
     * 
     * @since 3.0
     */
    public ActivationListener fActivationListener;
    public ISelectionChangedListener fPostSelectionListener;
    public OccurrencesFinderJob fOccurrencesFinderJob;
    /** The occurrences finder job canceler */
    public OccurrencesFinderJobCanceler fOccurrencesFinderJobCanceler;

    public MarkOccurencesHandler(final ErlangEditor erlangEditor,
            final Annotation[] fOccurrenceAnnotations,
            final long fMarkOccurrenceModificationStamp,
            final ActivationListener fActivationListener) {
        this.erlangEditor = erlangEditor;
        this.fOccurrenceAnnotations = fOccurrenceAnnotations;
        this.fMarkOccurrenceModificationStamp = fMarkOccurrenceModificationStamp;
        this.fActivationListener = fActivationListener;
    }

    protected void installOccurrencesFinder(final boolean forceUpdate) {
        fMarkOccurrenceAnnotations = true;

        fPostSelectionListener = new ISelectionChangedListener() {

            @Override
            public void selectionChanged(final SelectionChangedEvent event) {
                final ISelection selection = event.getSelection();
                erlangEditor.markOccurencesHandler.updateOccurrenceAnnotations(
                        (ITextSelection) selection, erlangEditor.getModule());
            }
        };
        final ISelectionProvider selectionProvider = erlangEditor
                .getSelectionProvider();
        if (selectionProvider != null) {
            ((IPostSelectionProvider) selectionProvider)
                    .addPostSelectionChangedListener(fPostSelectionListener);

            if (forceUpdate) {
                fForcedMarkOccurrencesSelection = selectionProvider
                        .getSelection();
                final IErlModule module = erlangEditor.getModule();
                if (module != null) {
                    erlangEditor.markOccurencesHandler
                            .updateOccurrenceAnnotations(
                                    (ITextSelection) fForcedMarkOccurrencesSelection,
                                    module);
                }
            }
        }
        if (fOccurrencesFinderJobCanceler == null) {
            fOccurrencesFinderJobCanceler = new OccurrencesFinderJobCanceler();
            fOccurrencesFinderJobCanceler.install();
        }
    }

    protected void uninstallOccurrencesFinder() {
        fMarkOccurrenceAnnotations = false;

        if (fOccurrencesFinderJob != null) {
            fOccurrencesFinderJob.cancel();
            fOccurrencesFinderJob = null;
        }

        if (fOccurrencesFinderJobCanceler != null) {
            fOccurrencesFinderJobCanceler.uninstall();
            fOccurrencesFinderJobCanceler = null;
        }

        if (fPostSelectionListener != null) {
            ((IPostSelectionProvider) erlangEditor.getSelectionProvider())
                    .removePostSelectionChangedListener(fPostSelectionListener);
            fPostSelectionListener = null;
        }

        erlangEditor.markOccurencesHandler.removeOccurrenceAnnotations();
    }

    protected boolean isMarkingOccurrences() {
        final IEclipsePreferences prefsNode = ErlideUIPlugin.getPrefsNode();
        return prefsNode.getBoolean("markingOccurences", false);
    }

    /**
     * Updates the occurrences annotations based on the current selection.
     * 
     * @param selection
     *            the text selection
     * @param module
     * @param astRoot
     *            the compilation unit AST
     * 
     * @since 3.0
     */
    protected void updateOccurrenceAnnotations(final ITextSelection selection,
            final IErlModule module) {

        if (fOccurrencesFinderJob != null) {
            fOccurrencesFinderJob.cancel();
        }

        if (!fMarkOccurrenceAnnotations) {
            return;
        }

        if (module == null || selection == null) {
            return;
        }

        final IDocument document = erlangEditor.getViewer().getDocument();
        if (document == null) {
            return;
        }

        boolean hasChanged = false;
        final int offset = selection.getOffset();
        if (document instanceof IDocumentExtension4) {
            final long currentModificationStamp = ((IDocumentExtension4) document)
                    .getModificationStamp();
            final IRegion markOccurrenceTargetRegion = fMarkOccurrenceTargetRegion;
            hasChanged = currentModificationStamp != fMarkOccurrenceModificationStamp;
            if (markOccurrenceTargetRegion != null && !hasChanged) {
                if (markOccurrenceTargetRegion.getOffset() <= offset
                        && offset <= markOccurrenceTargetRegion.getOffset()
                                + markOccurrenceTargetRegion.getLength()) {
                    return;
                }
            }
            fMarkOccurrenceTargetRegion = ErlangWordFinder.findWord(module,
                    erlangEditor, offset);
            fMarkOccurrenceModificationStamp = currentModificationStamp;
        }

        if (fOccurrencesFinderJob != null) {
            fOccurrencesFinderJob.cancel();
        }
        fOccurrencesFinderJob = new OccurrencesFinderJob(document, module,
                selection, hasChanged);
        fOccurrencesFinderJob.setPriority(Job.DECORATE);
        fOccurrencesFinderJob.setSystem(true);
        fOccurrencesFinderJob.schedule();
        // fOccurrencesFinderJob.run(new NullProgressMonitor());
    }

    void removeOccurrenceAnnotations() {
        fMarkOccurrenceModificationStamp = IDocumentExtension4.UNKNOWN_MODIFICATION_STAMP;
        fMarkOccurrenceTargetRegion = null;

        final IDocumentProvider documentProvider = erlangEditor
                .getDocumentProvider();
        if (documentProvider == null) {
            return;
        }

        final IAnnotationModel annotationModel = documentProvider
                .getAnnotationModel(erlangEditor.getEditorInput());
        if (annotationModel == null || fOccurrenceAnnotations == null) {
            return;
        }

        synchronized (erlangEditor.getLockObject(annotationModel)) {
            if (annotationModel instanceof IAnnotationModelExtension) {
                ((IAnnotationModelExtension) annotationModel)
                        .replaceAnnotations(fOccurrenceAnnotations, null);
            } else {
                for (int i = 0, length = fOccurrenceAnnotations.length; i < length; i++) {
                    annotationModel.removeAnnotation(fOccurrenceAnnotations[i]);
                }
            }
            fOccurrenceAnnotations = null;
        }
    }

    List<MarkOccurencesHandler.ErlangRef> getErlangRefs(
            final IErlModule module,
            final List<ModuleLineFunctionArityRef> findRefs) {
        final List<MarkOccurencesHandler.ErlangRef> result = new ArrayList<MarkOccurencesHandler.ErlangRef>(
                findRefs.size());
        for (final ModuleLineFunctionArityRef ref : findRefs) {
            result.add(new MarkOccurencesHandler.ErlangRef(SearchUtil
                    .createSearchElement(ref, module), ref.getOffset(), ref
                    .getLength(), ref.isDef()));
        }
        return result;
    }
}
