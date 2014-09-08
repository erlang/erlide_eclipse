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
import org.eclipse.ui.IWindowListener;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.services.search.ErlSearchScope;
import org.erlide.engine.services.search.ErlangSearchPattern;
import org.erlide.engine.services.search.LimitTo;
import org.erlide.engine.services.search.ModuleLineFunctionArityRef;
import org.erlide.engine.services.search.OpenResult;
import org.erlide.engine.services.search.OpenService;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.runtime.rpc.RpcTimeoutException;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.internal.search.ErlangSearchElement;
import org.erlide.ui.internal.search.SearchUtil;
import org.erlide.util.ErlLogger;
import org.erlide.util.IDisposable;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.google.common.collect.Lists;

public class MarkOccurencesSupport implements IDisposable {

    private final ErlangEditor editor;
    /**
     * Tells whether the occurrence annotations are sticky i.e. whether they
     * stay even if there's no valid erlang element at the current caret
     * position. Only valid if {@link #markOccurencesHandler.fMarkOccurrenceAnnotations} is
     * <code>true</code>.
     */
    public boolean fStickyOccurrenceAnnotations;
    public Annotation[] fOccurrenceAnnotations;
    /**
     * Tells whether all occurrences of the element at the current caret
     * location are automatically marked in this editor.
     */
    public boolean fMarkOccurrenceAnnotations;
    /**
     * The selection used when forcing occurrence marking through code.
     */
    public ISelection fForcedMarkOccurrencesSelection;
    public long fMarkOccurrenceModificationStamp;
    /**
     * The region of the word under the caret used to when computing the current
     * occurrence markings.
     */
    public IRegion fMarkOccurrenceTargetRegion;
    public ActivationListener fActivationListener;
    public ISelectionChangedListener fPostSelectionListener;
    public OccurrencesFinderJob fOccurrencesFinderJob;
    public OccurrencesFinderJobCanceler fOccurrencesFinderJobCanceler;

    public MarkOccurencesSupport(final ErlangEditor editor,
            final Annotation[] fOccurrenceAnnotations,
            final long fMarkOccurrenceModificationStamp) {
        this.editor = editor;
        this.fOccurrenceAnnotations = fOccurrenceAnnotations;
        this.fMarkOccurrenceModificationStamp = fMarkOccurrenceModificationStamp;
        fActivationListener = new ActivationListener();
    }

    protected void installOccurrencesFinder(final boolean forceUpdate) {
        fMarkOccurrenceAnnotations = true;

        fPostSelectionListener = new ISelectionChangedListener() {

            @Override
            public void selectionChanged(final SelectionChangedEvent event) {
                final ISelection selection = event.getSelection();
                editor.markOccurencesHandler.updateOccurrenceAnnotations(
                        (ITextSelection) selection, editor.getModule());
            }
        };
        final ISelectionProvider selectionProvider = editor.getSelectionProvider();
        if (selectionProvider != null) {
            ((IPostSelectionProvider) selectionProvider)
                    .addPostSelectionChangedListener(fPostSelectionListener);

            if (forceUpdate) {
                fForcedMarkOccurrencesSelection = selectionProvider.getSelection();
                final IErlModule module = editor.getModule();
                if (module != null) {
                    editor.markOccurencesHandler.updateOccurrenceAnnotations(
                            (ITextSelection) fForcedMarkOccurrencesSelection, module);
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
            ((IPostSelectionProvider) editor.getSelectionProvider())
                    .removePostSelectionChangedListener(fPostSelectionListener);
            fPostSelectionListener = null;
        }

        editor.markOccurencesHandler.removeOccurrenceAnnotations();
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

        final IDocument document = editor.getViewer().getDocument();
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
            fMarkOccurrenceTargetRegion = ErlangWordFinder.findWord(module, editor,
                    offset);
            fMarkOccurrenceModificationStamp = currentModificationStamp;
        }

        if (fOccurrencesFinderJob != null) {
            fOccurrencesFinderJob.cancel();
        }
        fOccurrencesFinderJob = new OccurrencesFinderJob(document, module, selection,
                hasChanged);
        fOccurrencesFinderJob.setPriority(Job.DECORATE);
        fOccurrencesFinderJob.setSystem(true);
        fOccurrencesFinderJob.schedule();
        // fOccurrencesFinderJob.run(new NullProgressMonitor());
    }

    void removeOccurrenceAnnotations() {
        fMarkOccurrenceModificationStamp = IDocumentExtension4.UNKNOWN_MODIFICATION_STAMP;
        fMarkOccurrenceTargetRegion = null;

        final IDocumentProvider documentProvider = editor.getDocumentProvider();
        if (documentProvider == null) {
            return;
        }

        final IAnnotationModel annotationModel = documentProvider
                .getAnnotationModel(editor.getEditorInput());
        if (annotationModel == null || fOccurrenceAnnotations == null) {
            return;
        }

        synchronized (editor.getLockObject(annotationModel)) {
            if (annotationModel instanceof IAnnotationModelExtension) {
                ((IAnnotationModelExtension) annotationModel).replaceAnnotations(
                        fOccurrenceAnnotations, null);
            } else {
                for (int i = 0, length = fOccurrenceAnnotations.length; i < length; i++) {
                    annotationModel.removeAnnotation(fOccurrenceAnnotations[i]);
                }
            }
            fOccurrenceAnnotations = null;
        }
    }

    List<MarkOccurencesSupport.ErlangRef> getErlangRefs(final IErlModule module,
            final List<ModuleLineFunctionArityRef> findRefs) {
        final List<MarkOccurencesSupport.ErlangRef> result = new ArrayList<MarkOccurencesSupport.ErlangRef>(
                findRefs.size());
        for (final ModuleLineFunctionArityRef ref : findRefs) {
            result.add(new MarkOccurencesSupport.ErlangRef(SearchUtil
                    .createSearchElement(ref, module), ref.getOffset(), ref.getLength(),
                    ref.isDef()));
        }
        return result;
    }

    @Override
    public void dispose() {
        fMarkOccurrenceAnnotations = false;
        uninstallOccurrencesFinder();
        if (fActivationListener != null) {
            PlatformUI.getWorkbench().removeWindowListener(fActivationListener);
            fActivationListener = null;
        }
    }

    public void setEnabled(final boolean newBooleanValue) {
        if (newBooleanValue != fMarkOccurrenceAnnotations) {
            fMarkOccurrenceAnnotations = newBooleanValue;
            if (!fMarkOccurrenceAnnotations) {
                uninstallOccurrencesFinder();
            } else {
                installOccurrencesFinder(true);
            }
        }
    }

    class OccurrencesFinderJob extends Job {

        private final IDocument fDocument;
        private final ITextSelection selection;
        private final ISelectionValidator fPostSelectionValidator;
        private boolean fCanceled = false;
        private List<ErlangRef> fRefs;
        private final boolean fHasChanged;
        private final IErlModule module;

        public OccurrencesFinderJob(final IDocument document, final IErlModule module,
                final ITextSelection selection, final boolean hasChanged) {
            super("OccurrencesFinderJob");
            fDocument = document;
            this.selection = selection;

            if (editor.getSelectionProvider() instanceof ISelectionValidator) {
                fPostSelectionValidator = (ISelectionValidator) editor
                        .getSelectionProvider();
            } else {
                fPostSelectionValidator = null;
            }
            this.module = module;
            fHasChanged = hasChanged;
        }

        private void findRefs(final IErlModule theModule,
                final ITextSelection aSelection, final boolean hasChanged) {
            fRefs = null;

            if (fCanceled) {
                return;
            }
            try {
                final int offset = aSelection.getOffset();
                final OpenResult res = ErlangEngine
                        .getInstance()
                        .getService(OpenService.class)
                        .open(theModule.getScannerName(),
                                offset,
                                ErlangEngine.getInstance().getModelUtilService()
                                        .getImportsAsList(theModule), "",
                                ErlangEngine.getInstance().getModel().getPathVars());
                final ErlangSearchPattern pattern = SearchUtil
                        .getSearchPatternFromOpenResultAndLimitTo(theModule, offset, res,
                                LimitTo.ALL_OCCURRENCES, false);
                if (fCanceled) {
                    return;
                }
                if (pattern != null) {
                    final ErlSearchScope scope = new ErlSearchScope();
                    scope.addModule(theModule);
                    final List<ModuleLineFunctionArityRef> findRefs = Lists
                            .newArrayList();
                    // TODO: should run in background
                    final OtpErlangObject refs = ErlangEngine
                            .getInstance()
                            .getSearchServerService()
                            .findRefs(pattern, scope,
                                    ErlangEngine.getInstance().getStateDir(), true);
                    if (refs != null) {
                        SearchUtil.addSearchResult(findRefs, refs);
                        fRefs = editor.markOccurencesHandler.getErlangRefs(theModule,
                                findRefs);
                    }
                }
            } catch (final RpcTimeoutException e) {
                ErlLogger.warn(e);
            } catch (final RpcException e) {
                ErlLogger.warn(e);
            } catch (final ErlModelException e) {
                ErlLogger.warn(e);
            } catch (final OtpErlangRangeException e) {
                ErlLogger.warn(e);
            }
            if (fRefs == null) {
                if (!editor.markOccurencesHandler.fStickyOccurrenceAnnotations) {
                    editor.markOccurencesHandler.removeOccurrenceAnnotations();
                } else if (hasChanged) {
                    editor.markOccurencesHandler.removeOccurrenceAnnotations();
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
                    && !(fPostSelectionValidator.isValid(selection) || editor.markOccurencesHandler.fForcedMarkOccurrencesSelection == selection)
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

            final ITextViewer textViewer = editor.getViewer();
            if (textViewer == null) {
                return Status.CANCEL_STATUS;
            }

            final IDocument document = textViewer.getDocument();
            if (document == null) {
                return Status.CANCEL_STATUS;
            }

            final IDocumentProvider documentProvider = editor.getDocumentProvider();
            if (documentProvider == null) {
                return Status.CANCEL_STATUS;
            }

            final IAnnotationModel annotationModel = documentProvider
                    .getAnnotationModel(editor.getEditorInput());
            if (annotationModel == null) {
                return Status.CANCEL_STATUS;
            }

            // Add occurrence annotations
            final HashMap<Annotation, Position> annotationMap = new HashMap<Annotation, Position>(
                    fRefs.size());
            for (final MarkOccurencesSupport.ErlangRef ref : fRefs) {
                if (isCanceled(progressMonitor)) {
                    return Status.CANCEL_STATUS;
                }

                final Position position = new Position(ref.getOffset(), ref.getLength());

                final String description = ref.getDescription();
                final String annotationType = ref.isDef() ? "org.erlide.ui.occurrences.definition" //$NON-NLS-1$
                        : "org.erlide.ui.occurrences";

                annotationMap.put(new Annotation(annotationType, false, description),
                        position);
            }

            if (isCanceled(progressMonitor)) {
                return Status.CANCEL_STATUS;
            }

            synchronized (editor.getLockObject(annotationModel)) {
                if (annotationModel instanceof IAnnotationModelExtension) {
                    ((IAnnotationModelExtension) annotationModel).replaceAnnotations(
                            editor.markOccurencesHandler.fOccurrenceAnnotations,
                            annotationMap);
                } else {
                    editor.markOccurencesHandler.removeOccurrenceAnnotations();
                    for (final Map.Entry<Annotation, Position> mapEntry : annotationMap
                            .entrySet()) {
                        annotationModel.addAnnotation(mapEntry.getKey(),
                                mapEntry.getValue());
                    }
                }
                editor.markOccurencesHandler.fOccurrenceAnnotations = annotationMap
                        .keySet().toArray(new Annotation[annotationMap.keySet().size()]);
            }

            return Status.OK_STATUS;
        }
    }

    class OccurrencesFinderJobCanceler implements IDocumentListener, ITextInputListener {

        public void install() {
            final ISourceViewer sourceViewer = editor.getViewer();
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
            final ISourceViewer sourceViewer = editor.getViewer();
            if (sourceViewer != null) {
                sourceViewer.removeTextInputListener(this);
            }

            final IDocumentProvider documentProvider = editor.getDocumentProvider();
            if (documentProvider != null) {
                final IDocument document = documentProvider.getDocument(editor
                        .getEditorInput());
                if (document != null) {
                    document.removeDocumentListener(this);
                }
            }
        }

        @Override
        public void documentAboutToBeChanged(final DocumentEvent event) {
            if (editor.markOccurencesHandler.fOccurrencesFinderJob != null) {
                editor.markOccurencesHandler.fOccurrencesFinderJob.doCancel();
            }
        }

        @Override
        public void documentChanged(final DocumentEvent event) {
        }

        @Override
        public void inputDocumentAboutToBeChanged(final IDocument oldInput,
                final IDocument newInput) {
            if (oldInput == null) {
                return;
            }

            oldInput.removeDocumentListener(this);
        }

        @Override
        public void inputDocumentChanged(final IDocument oldInput,
                final IDocument newInput) {
            if (newInput == null) {
                return;
            }
            newInput.addDocumentListener(this);
        }
    }

    private static class ErlangRef {
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

        public int getOffset() {
            return offset;
        }

        public int getLength() {
            return length;
        }

        public boolean isDef() {
            return def;
        }

        public String getDescription() {
            return element.toString();
        }

    }

    private class ActivationListener implements IWindowListener {
        @Override
        public void windowActivated(final IWorkbenchWindow window) {
            if (window == editor.getEditorSite().getWorkbenchWindow()
                    && fMarkOccurrenceAnnotations && editor.isActivePart()) {
                fForcedMarkOccurrencesSelection = editor.getSelectionProvider()
                        .getSelection();

                updateOccurrenceAnnotations(
                        (ITextSelection) fForcedMarkOccurrencesSelection,
                        editor.getModule());
            }
        }

        @Override
        public void windowDeactivated(final IWorkbenchWindow window) {
            if (window == editor.getEditorSite().getWorkbenchWindow()
                    && fMarkOccurrenceAnnotations && editor.isActivePart()) {
                removeOccurrenceAnnotations();
            }
        }

        @Override
        public void windowClosed(final IWorkbenchWindow window) {
        }

        @Override
        public void windowOpened(final IWorkbenchWindow window) {
        }
    }
}
