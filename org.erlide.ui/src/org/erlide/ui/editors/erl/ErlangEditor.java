/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *     Vlad Dumitrescu
 *     Alain O'Dea
 *******************************************************************************/
package org.erlide.ui.editors.erl;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentExtension4;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ISynchronizable;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.ITextViewerExtension5;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.ICharacterPairMatcher;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.projection.ProjectionSupport;
import org.eclipse.jface.text.source.projection.ProjectionViewer;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.IPostSelectionProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IPartService;
import org.eclipse.ui.IPathEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionContext;
import org.eclipse.ui.actions.ActionGroup;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;
import org.eclipse.ui.texteditor.ChainedPreferenceStore;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.IEditorStatusLine;
import org.eclipse.ui.texteditor.ITextEditorActionConstants;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.eclipse.ui.views.properties.IPropertySource;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.erlang.IErlAttribute;
import org.erlide.engine.model.erlang.IErlFunctionClause;
import org.erlide.engine.model.erlang.IErlMember;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.ISourceRange;
import org.erlide.engine.model.erlang.ISourceReference;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.parsing.ScannerService;
import org.erlide.engine.services.search.XrefService;
import org.erlide.ui.actions.CompositeActionGroup;
import org.erlide.ui.actions.ErlangSearchActionGroup;
import org.erlide.ui.editors.erl.actions.CallHierarchyAction;
import org.erlide.ui.editors.erl.actions.CleanUpAction;
import org.erlide.ui.editors.erl.actions.ClearCacheAction;
import org.erlide.ui.editors.erl.actions.CompileAction;
import org.erlide.ui.editors.erl.actions.GotoMatchingBracketAction;
import org.erlide.ui.editors.erl.actions.ShowOutlineAction;
import org.erlide.ui.editors.erl.folding.IErlangFoldingStructureProvider;
import org.erlide.ui.editors.erl.folding.IErlangFoldingStructureProviderExtension;
import org.erlide.ui.editors.erl.outline.ErlangContentProvider;
import org.erlide.ui.editors.erl.outline.ErlangLabelProvider;
import org.erlide.ui.editors.erl.outline.ErlangOutlinePage;
import org.erlide.ui.editors.erl.outline.IOutlineContentCreator;
import org.erlide.ui.editors.erl.outline.IOutlineSelectionHandler;
import org.erlide.ui.editors.erl.outline.ISortableContentOutlinePage;
import org.erlide.ui.editors.erl.test.TestAction;
import org.erlide.ui.editors.util.EditorUtility;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.prefs.PreferenceConstants;
import org.erlide.ui.util.ErlModelUtils;
import org.erlide.ui.util.ProblemsLabelDecorator;
import org.erlide.ui.views.ErlangPropertySource;
import org.erlide.util.ErlLogger;
import org.erlide.util.IDisposable;
import org.erlide.util.SystemConfiguration;

/**
 * The actual editor itself
 *
 *
 * @author Eric Merrit [cyberlync at gmail dot com]
 */
public class ErlangEditor extends AbstractErlangEditor implements IOutlineContentCreator,
        IOutlineSelectionHandler {

    public static final String ERLANG_EDITOR_ID = "org.erlide.ui.editors.erl.ErlangEditor";
    public static final String EDITOR_INDENT_WIDTH = "indentWidth";

    private IErlModule fModule = null;
    private ColorManager colorManager;
    private ErlangOutlinePage myOutlinePage;
    private IPropertySource myPropertySource;
    private ProjectionSupport fProjectionSupport;
    private IErlangFoldingStructureProvider fProjectionModelUpdater;
    private final ErlangEditorErrorTickUpdater fErlangEditorErrorTickUpdater;
    private ToggleFoldingRunner fFoldingRunner;

    private EditorSelectionChangedListener fEditorSelectionChangedListener;
    private final IPreferenceChangeListener fPreferenceChangeListener = new PreferenceChangeListener();
    private final IPropertyChangeListener propertyChangeListener = new PropertyChangeListener();

    private Object fSelection;
    private ActionGroup fActionGroups;
    private ActionGroup fContextMenuGroup;
    private ShowOutlineAction fShowOutline;
    private CompileAction compileAction;
    private CleanUpAction cleanUpAction;
    private ClearCacheAction clearCacheAction;
    private CallHierarchyAction callhierarchy;
    private TestAction testAction;

    private final AnnotationSupport annotationSupport;

    XrefService xrefService;

    final MarkOccurencesSupport markOccurencesHandler = new MarkOccurencesSupport(this,
            null, IDocumentExtension4.UNKNOWN_MODIFICATION_STAMP);

    public ErlangEditor(final XrefService xrefService) {
        super();
        fErlangEditorErrorTickUpdater = new ErlangEditorErrorTickUpdater(this);

        annotationSupport = new AnnotationSupport(this, getAnnotationPreferenceLookup());
        this.xrefService = xrefService;

        setRulerContextMenuId("#ErlangEditorRulerContext");
    }

    @Override
    public void dispose() {
        if (colorManager != null) {
            colorManager.dispose();
            colorManager = null;
        }

        final ISourceViewer sourceViewer = getSourceViewer();
        if (sourceViewer instanceof IDisposable) {
            ((IDisposable) sourceViewer).dispose();
        }

        final IEclipsePreferences node = ErlideUIPlugin.getPrefsNode();
        node.removePreferenceChangeListener(fPreferenceChangeListener);
        ErlideUIPlugin.getDefault().getPreferenceStore()
                .removePropertyChangeListener(propertyChangeListener);
        if (fActionGroups != null) {
            fActionGroups.dispose();
            fActionGroups = null;
        }
        fErlangEditorErrorTickUpdater.dispose();
        if (fProjectionModelUpdater != null) {
            fProjectionModelUpdater.uninstall();
        }

        disposeModule();

        // cancel possible running computation
        markOccurencesHandler.dispose();
        if (getSourceViewerConfiguration() instanceof IDisposable) {
            ((IDisposable) getSourceViewerConfiguration()).dispose();
        }

        super.dispose();
    }

    @Override
    protected void initializeEditor() {
        colorManager = new ColorManager();
        setDocumentProvider(new TextFileDocumentProvider());

        final IPreferenceStore store = getErlangEditorPreferenceStore();
        setPreferenceStore(store);

        final ErlangSourceViewerConfiguration cfg = new EditorConfiguration(
                getPreferenceStore(), this, colorManager);
        setSourceViewerConfiguration(cfg);
    }

    public static IPreferenceStore getErlangEditorPreferenceStore() {
        final IPreferenceStore generalTextStore = EditorsUI.getPreferenceStore();
        return new ChainedPreferenceStore(new IPreferenceStore[] {
                ErlideUIPlugin.getDefault().getPreferenceStore(), generalTextStore });
    }

    public void disposeModule() {
        if (fModule != null) {
            fModule.dispose();
            fModule = null;
        }
    }

    @Override
    protected void initializeKeyBindingScopes() {
        setKeyBindingScopes(new String[] { "org.erlide.ui.erlangEditorScope" }); //$NON-NLS-1$
    }

    class PreferenceChangeListener implements IPreferenceChangeListener {
        @Override
        public void preferenceChange(final PreferenceChangeEvent event) {
            final String key = event.getKey();
            if ("markingOccurences".equals(key)) {
                final boolean newBooleanValue = event.getNewValue().equals("true");
                markOccurencesHandler.setEnabled(newBooleanValue);
            }
        }
    }

    class PropertyChangeListener implements IPropertyChangeListener {
        @Override
        public void propertyChange(final PropertyChangeEvent event) {
            if (getSourceViewerConfiguration() instanceof ErlangSourceViewerConfiguration) {

                final ErlangSourceViewerConfiguration configuration = (ErlangSourceViewerConfiguration) getSourceViewerConfiguration();
                if (configuration.affectsTextPresentation(event)) {
                    configuration.handlePropertyChangeEvent(event);
                    getSourceViewer().invalidateTextPresentation();
                }
            }
        }
    }

    protected final boolean isActiveEditor() {
        final IWorkbenchWindow window = getSite().getWorkbenchWindow();
        final IWorkbenchPage page = window.getActivePage();
        if (page == null) {
            return false;
        }
        final IEditorPart activeEditor = page.getActiveEditor();
        return activeEditor != null && activeEditor.equals(this);
    }

    @Override
    protected void createActions() {
        super.createActions();
        ActionGroup esg;
        fActionGroups = new CompositeActionGroup(
                new ActionGroup[] { esg = new ErlangSearchActionGroup(this) });
        fContextMenuGroup = new CompositeActionGroup(new ActionGroup[] { esg });

        createCommonActions();

        compileAction = new CompileAction(getSite());
        compileAction.setActionDefinitionId(IErlangEditorActionDefinitionIds.COMPILE);
        setAction("compileFile", compileAction);

        if (getModule() != null) {
            cleanUpAction = new CleanUpAction(getModule().getResource());
            cleanUpAction
                    .setActionDefinitionId(IErlangEditorActionDefinitionIds.CLEAN_UP);
            setAction("cleanUp", cleanUpAction);
        }

        if (SystemConfiguration.getInstance().isTest()) {
            setupTestAction();
            // PlatformUI.getWorkbench().getHelpSystem().setHelp(indentAction,
            // IErlangHelpContextIds.INDENT_ACTION);
        }

        callhierarchy = new CallHierarchyAction(this, getModule(), xrefService);
        callhierarchy
                .setActionDefinitionId(IErlangEditorActionDefinitionIds.CALLHIERARCHY);
        setAction("callHierarchy", callhierarchy);
        markAsStateDependentAction("CallHierarchy", true);
        markAsSelectionDependentAction("CallHierarchy", true);

        if (SystemConfiguration.getInstance().isClearCacheAvailable()) {
            setupClearCacheAction();
            // PlatformUI.getWorkbench().getHelpSystem().setHelp(indentAction,
            // IErlangHelpContextIds.INDENT_ACTION);
        }

        fShowOutline = new ShowOutlineAction(
                ErlangEditorMessages.getBundleForConstructedKeys(), "ShowOutline.", this);
        fShowOutline.setActionDefinitionId(IErlangEditorActionDefinitionIds.SHOW_OUTLINE);
        setAction(IErlangEditorActionDefinitionIds.SHOW_OUTLINE, fShowOutline);
        markAsContentDependentAction(IErlangEditorActionDefinitionIds.SHOW_OUTLINE, true);

        final Action action = new GotoMatchingBracketAction(this);
        action.setActionDefinitionId(IErlangEditorActionDefinitionIds.GOTO_MATCHING_BRACKET);
        setAction(GotoMatchingBracketAction.GOTO_MATCHING_BRACKET, action);

    }

    private void setupClearCacheAction() {
        if (clearCacheAction != null) {
            return;
        }
        clearCacheAction = new ClearCacheAction(
                ErlangEditorMessages.getBundleForConstructedKeys(), "ClearCache.", this);
        clearCacheAction
                .setActionDefinitionId(IErlangEditorActionDefinitionIds.CLEAR_CACHE);
        setAction("ClearCache", clearCacheAction);
        markAsStateDependentAction("ClearCache", true);
        markAsSelectionDependentAction("ClearCache", true);
    }

    private void setupTestAction() {
        if (testAction != null) {
            return;
        }
        testAction = new TestAction(ErlangEditorMessages.getBundleForConstructedKeys(),
                "Test.", this, getModule());
        testAction.setActionDefinitionId(IErlangEditorActionDefinitionIds.TEST);
        setAction("Test", testAction);
        markAsStateDependentAction("Test", true);
        markAsSelectionDependentAction("Test", true);
    }

    @Override
    protected void editorContextMenuAboutToShow(final IMenuManager menu) {
        super.editorContextMenuAboutToShow(menu);

        if (SystemConfiguration.getInstance().isTest()) {
            setupTestAction();
            menu.prependToGroup(ITextEditorActionConstants.GROUP_OPEN, testAction);
        }
        if (SystemConfiguration.getInstance().isClearCacheAvailable()) {
            setupClearCacheAction();
            menu.prependToGroup(ITextEditorActionConstants.GROUP_SETTINGS,
                    clearCacheAction);
        }
        menu.prependToGroup(ITextEditorActionConstants.GROUP_OPEN, compileAction);
        menu.prependToGroup(ITextEditorActionConstants.GROUP_OPEN, fShowOutline);
        addCommonActions(menu);

        menu.appendToGroup(ITextEditorActionConstants.GROUP_FIND, callhierarchy);

        final ActionContext context = new ActionContext(getSelectionProvider()
                .getSelection());
        fContextMenuGroup.setContext(context);
        fContextMenuGroup.fillContextMenu(menu);
        fContextMenuGroup.setContext(null);
    }

    @Override
    public Object getAdapter(final Class required) {
        if (IContentOutlinePage.class.equals(required)) {
            if (myOutlinePage == null) {
                myOutlinePage = createOutlinePage();
            }
            return myOutlinePage;
        }
        if (IPropertySource.class.equals(required)) {
            if (myPropertySource == null) {
                myPropertySource = new ErlangPropertySource(this);
            }
            return myPropertySource;
        }

        if (IErlangFoldingStructureProvider.class.equals(required)) {
            return fProjectionModelUpdater;
        }

        if (fProjectionSupport != null) {
            final Object adapter = fProjectionSupport.getAdapter(getSourceViewer(),
                    required);
            if (adapter != null) {
                return adapter;
            }
        }

        return super.getAdapter(required);
    }

    public IDocument getDocument() {
        return getSourceViewer().getDocument();
    }

    /**
     * Jumps to the matching bracket.
     */
    public void gotoMatchingBracket() {

        final ISourceViewer sourceViewer = getSourceViewer();
        final IDocument document = sourceViewer.getDocument();
        if (document == null) {
            return;
        }

        final IRegion selection = getSignedSelection(sourceViewer);

        final int selectionLength = Math.abs(selection.getLength());
        if (selectionLength > 1) {
            setStatusLineErrorMessage(ErlangEditorMessages.GotoMatchingBracket_error_invalidSelection);
            sourceViewer.getTextWidget().getDisplay().beep();
            return;
        }

        // #26314
        final int sourceCaretOffset = selection.getOffset() + selection.getLength();
        // TODO fix me!
        // if (isSurroundedByBrackets(document, sourceCaretOffset))
        // sourceCaretOffset -= selection.getLength();

        final IRegion region = getBracketMatcher().match(document, sourceCaretOffset);
        if (region == null) {
            setStatusLineErrorMessage(ErlangEditorMessages.GotoMatchingBracket_error_noMatchingBracket);
            sourceViewer.getTextWidget().getDisplay().beep();
            return;
        }

        final int offset = region.getOffset();
        final int length = region.getLength();

        if (length < 1) {
            return;
        }

        final int anchor = getBracketMatcher().getAnchor();
        // http://dev.eclipse.org/bugs/show_bug.cgi?id=34195
        int targetOffset = ICharacterPairMatcher.RIGHT == anchor ? offset + 1 : offset
                + length;

        boolean visible = false;
        if (sourceViewer instanceof ITextViewerExtension5) {
            final ITextViewerExtension5 extension = (ITextViewerExtension5) sourceViewer;
            visible = extension.modelOffset2WidgetOffset(targetOffset) > -1;
        } else {
            final IRegion visibleRegion = sourceViewer.getVisibleRegion();
            // http://dev.eclipse.org/bugs/show_bug.cgi?id=34195
            visible = targetOffset >= visibleRegion.getOffset()
                    && targetOffset <= visibleRegion.getOffset()
                            + visibleRegion.getLength();
        }

        if (!visible) {
            setStatusLineErrorMessage(ErlangEditorMessages.GotoMatchingBracket_error_bracketOutsideSelectedElement);
            sourceViewer.getTextWidget().getDisplay().beep();
            return;
        }

        if (selection.getLength() < 0) {
            targetOffset -= selection.getLength();
        }

        sourceViewer.setSelectedRange(targetOffset, selection.getLength());
        sourceViewer.revealRange(targetOffset, selection.getLength());
    }

    /**
     * Returns the signed current selection. The length will be negative if the
     * resulting selection is right-to-left (RtoL).
     * <p>
     * The selection offset is model based.
     * </p>
     *
     * @param sourceViewer
     *            the source viewer
     * @return a region denoting the current signed selection, for a resulting
     *         RtoL selections length is < 0
     */
    protected IRegion getSignedSelection(final ISourceViewer sourceViewer) {
        final StyledText text = sourceViewer.getTextWidget();
        final Point selection = text.getSelectionRange();

        if (text.getCaretOffset() == selection.x) {
            selection.x = selection.x + selection.y;
            selection.y = -selection.y;
        }

        selection.x = widgetOffset2ModelOffset(sourceViewer, selection.x);

        return new Region(selection.x, selection.y);
    }

    /**
     * Sets the given message as error message to this editor's status line.
     *
     * @param msg
     *            message to be set
     */
    @Override
    protected void setStatusLineErrorMessage(final String msg) {
        final IEditorStatusLine statusLine = (IEditorStatusLine) getAdapter(IEditorStatusLine.class);
        if (statusLine != null) {
            statusLine.setMessage(true, msg, null);
        }
    }

    /**
     * Sets the given message as message to this editor's status line.
     *
     * @param msg
     *            message to be set
     * @since 3.0
     */
    @Override
    protected void setStatusLineMessage(final String msg) {
        final IEditorStatusLine statusLine = (IEditorStatusLine) getAdapter(IEditorStatusLine.class);
        if (statusLine != null) {
            statusLine.setMessage(false, msg, null);
        }
    }

    @Override
    protected void doSetInput(final IEditorInput input) throws CoreException {
        final IDocumentProvider provider = getDocumentProvider();
        if (input != getEditorInput()) {
            disposeModule();
            resetReconciler();
        }

        super.doSetInput(input);

        final IDocument document = provider.getDocument(input);
        if (!(input instanceof IPathEditorInput) && document != null) {
            final ErlangDocumentSetupParticipant setupParticipant = new ErlangDocumentSetupParticipant();
            setupParticipant.setup(document);
        }

        if (myOutlinePage != null) {
            // TODO should we use model events here?
            myOutlinePage.setInput(input);
        }
        final IErlModule module = getModule();
        if (module != null) {
            fErlangEditorErrorTickUpdater.updateEditorImage(module);
        }

        if (document != null) {
            // scannerListener = new ScannerListener();
            // document.addDocumentListener(scannerListener);
            // scannerListener.documentOpened();
            if (document.getLength() > 0) {
                // fake a change if the document already has content
                // scannerListener.documentChanged(new DocumentEvent(document,
                // 0,
                // document.getLength(), document.get()));
            }
        }

    }

    protected ISourceReference computeHighlightRangeSourceReference() {
        final ISourceViewer sourceViewer = getSourceViewer();
        if (sourceViewer == null) {
            return null;
        }

        final StyledText styledText = sourceViewer.getTextWidget();
        if (styledText == null) {
            return null;
        }

        int caret = 0;
        if (sourceViewer instanceof ITextViewerExtension5) {
            final ITextViewerExtension5 extension = (ITextViewerExtension5) sourceViewer;
            caret = extension.widgetOffset2ModelOffset(styledText.getCaretOffset());
        } else {
            final int offset = sourceViewer.getVisibleRegion().getOffset();
            caret = offset + styledText.getCaretOffset();
        }

        final IErlElement element = getElementAt(caret, false);

        if (!(element instanceof ISourceReference)) {
            return null;
        }

        return (ISourceReference) element;
    }

    @Override
    public IErlModule getModule() {
        if (fModule == null) {
            try {
                fModule = ErlModelUtils.getModule(getEditorInput());
                fModule.createScanner();
                final ScannerService erlScanner = fModule.getScanner();
                erlScanner.dispose();
            } catch (final CoreException e) {
            }
        }
        return fModule;
    }

    @Override
    protected void addFoldingSupport(final ISourceViewer viewer) {
        if (EditorUtility.isFoldingEnabled()) {
            final ProjectionViewer projectionViewer = (ProjectionViewer) viewer;
            fProjectionSupport = new ProjectionSupport(projectionViewer,
                    getAnnotationAccess(), getSharedColors());
            fProjectionSupport
                    .addSummarizableAnnotationType("org.eclipse.ui.workbench.texteditor.error"); //$NON-NLS-1$
            fProjectionSupport
                    .addSummarizableAnnotationType("org.eclipse.ui.workbench.texteditor.warning"); //$NON-NLS-1$
            fProjectionSupport.install();

            fProjectionModelUpdater = ErlideUIPlugin.getDefault()
                    .getFoldingStructureProviderRegistry().getCurrentFoldingProvider();
            if (fProjectionModelUpdater != null) {
                fProjectionModelUpdater.install(this, projectionViewer);
            }
        }
    }

    /**
     * Returns the most narrow element including the given offset. If
     * <code>reconcile</code> is <code>true</code> the editor's input element is
     * reconciled in advance. If it is <code>false</code> this method only
     * returns a result if the editor's input element does not need to be
     * reconciled.
     *
     * @param offset
     *            the offset included by the retrieved element
     * @param reconcile
     *            <code>true</code> if working copy should be reconciled
     * @return the most narrow element which includes the given offset
     * @throws ErlModelException
     */
    @Override
    public IErlElement getElementAt(final int offset, final boolean reconcile) {
        final IErlModule module = getModule();
        if (module == null) {
            return null;
        }
        try {
            if (reconcile) {
                synchronized (module) {
                    module.open(null);
                    return module.getElementAt(offset);
                }
            } else if (module.isConsistent()) {
                return module.getElementAt(offset);
            }
        } catch (final Exception e) {
        }
        return null;
    }

    protected abstract class AbstractSelectionChangedListener implements
            ISelectionChangedListener {

        /**
         * Installs this selection changed listener with the given selection
         * provider. If the selection provider is a post selection provider,
         * post selection changed events are the preferred choice, otherwise
         * normal selection changed events are requested.
         *
         * @param selectionProvider
         */
        public void install(final ISelectionProvider selectionProvider) {
            if (selectionProvider == null) {
                return;
            }

            if (selectionProvider instanceof IPostSelectionProvider) {
                final IPostSelectionProvider provider = (IPostSelectionProvider) selectionProvider;
                provider.addPostSelectionChangedListener(this);
            } else {
                selectionProvider.addSelectionChangedListener(this);
            }
        }

        /**
         * Removes this selection changed listener from the given selection
         * provider.
         *
         * @param selectionProvider
         *            the selection provider
         */
        public void uninstall(final ISelectionProvider selectionProvider) {
            if (selectionProvider == null) {
                return;
            }

            if (selectionProvider instanceof IPostSelectionProvider) {
                final IPostSelectionProvider provider = (IPostSelectionProvider) selectionProvider;
                provider.removePostSelectionChangedListener(this);
            } else {
                selectionProvider.removeSelectionChangedListener(this);
            }

        }
    }

    /**
     * Updates the Erlang outline page selection and this editor's range
     * indicator.
     */
    class EditorSelectionChangedListener extends AbstractSelectionChangedListener {

        /*
         * @see
         * org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged
         * (org.eclipse.jface.viewers.SelectionChangedEvent)
         */
        @Override
        public void selectionChanged(final SelectionChangedEvent event) {
            ErlangEditor.this.selectionChanged();
        }
    }

    /**
     * Called from
     * org.erlide.ui.editors.erl.outline.ErlangOutlinePage.createControl
     * (...).new OpenAndLinkWithEditorHelper() {...}.linkToEditor(ISelection)
     *
     * @param selection
     */
    public void doSelectionChanged(final ISelection selection) {
        ISourceReference reference = null;
        if (selection instanceof IStructuredSelection) {
            final IStructuredSelection ss = (IStructuredSelection) selection;
            for (final Object o : ss.toArray()) {
                if (o instanceof ISourceReference) {
                    reference = (ISourceReference) o;
                    break;
                }
            }
        }
        if (!isActivePart() && ErlideUIPlugin.getActivePage() != null) {
            ErlideUIPlugin.getActivePage().bringToTop(this);
        }
        setSelection(reference, true);
    }

    protected void selectionChanged() {
        if (getSelectionProvider() == null) {
            return;
        }
        final ISourceReference element = computeHighlightRangeSourceReference();
        if (isLinkedToOutlinePage()) {
            synchronizeOutlinePage(element);
        }
        setSelection(element, false);
        // updateStatusLine();
    }

    private boolean isLinkedToOutlinePage() {
        return myOutlinePage.isLinkedWithEditor();
    }

    /**
     * Creates the outline page used with this editor.
     *
     * @return the created Erlang outline page
     */
    protected ErlangOutlinePage createOutlinePage() {
        final ErlangOutlinePage page = new ErlangOutlinePage(this);
        page.setInput(getEditorInput());
        return page;
    }

    /**
     * Informs the editor that its outline has been closed.
     */
    public void outlinePageClosed() {
        if (myOutlinePage != null) {
            myOutlinePage = null;
            resetHighlightRange();
        }
    }

    /**
     * @see org.eclipse.ui.part.WorkbenchPart#setTitleImage(Image titleImage);
     * @param image
     */
    public void updatedTitleImage(final Image image) {
        setTitleImage(image);
    }

    /**
     * Synchronizes the outliner selection with the given element position in
     * the editor.
     *
     * @param element
     *            the java element to select
     */
    protected void synchronizeOutlinePage(final ISourceReference element) {
        synchronizeOutlinePage(element, true);
    }

    /**
     * Synchronizes the outliner selection with the given element position in
     * the editor.
     *
     * @param element
     *            the java element to select
     * @param checkIfOutlinePageActive
     *            <code>true</code> if check for active outline page needs to be
     *            done
     */
    protected void synchronizeOutlinePage(final ISourceReference element,
            final boolean checkIfOutlinePageActive) {
        if (myOutlinePage != null) {
            myOutlinePage.select(element);
        }
    }

    /**
     * Synchronizes the outliner selection with the actual cursor position in
     * the editor.
     */
    public void synchronizeOutlinePageSelection() {
        synchronizeOutlinePage(computeHighlightRangeSourceReference());
    }

    public void setSelection(final ISourceReference reference, final boolean moveCursor) {
        if (getSelectionProvider() == null) {
            return;
        }

        final ISelection selection = getSelectionProvider().getSelection();
        if (selection instanceof TextSelection) {
            final TextSelection textSelection = (TextSelection) selection;
            if (moveCursor
                    && (textSelection.getOffset() != 0 || textSelection.getLength() != 0)) {
                markInNavigationHistory();
            }
        }

        if (reference != null) {

            StyledText textWidget = null;

            final ISourceViewer sourceViewer = getSourceViewer();
            if (sourceViewer == null) {
                return;
            }
            textWidget = sourceViewer.getTextWidget();

            if (textWidget == null) {
                return;
            }

            try {
                ISourceRange range = null;
                range = reference.getSourceRange();

                if (range == null) {
                    return;
                }

                int offset = range.getOffset();
                int length = range.getLength();

                if (offset < 0 || length < 0) {
                    return;
                }

                setHighlightRange(offset, length, moveCursor);

                if (!moveCursor) {
                    return;
                }

                offset = -1;
                length = -1;

                if (reference instanceof IErlMember) {
                    range = ((IErlMember) reference).getNameRange();
                    if (range != null) {
                        offset = range.getOffset();
                        length = range.getLength();
                    }
                } else if (reference instanceof IErlAttribute) {
                    range = ((IErlAttribute) reference).getNameRange();
                    if (range != null) {
                        offset = range.getOffset();
                        length = range.getLength();
                    }

                } else if (reference instanceof IErlFunctionClause) {
                    range = ((IErlFunctionClause) reference).getNameRange();
                    if (range != null) {
                        offset = range.getOffset();
                        length = range.getLength();
                    }
                }
                if (offset > -1 && length > 0) {

                    try {
                        textWidget.setRedraw(false);
                        sourceViewer.revealRange(offset, length);
                        sourceViewer.setSelectedRange(offset, length);
                    } finally {
                        textWidget.setRedraw(true);
                    }

                    markInNavigationHistory();
                }

            } catch (final IllegalArgumentException x) {
            }

        } else if (moveCursor) {
            resetHighlightRange();
            markInNavigationHistory();
        }
    }

    public void setSelection(final IErlElement element) {

        if (element == null || element instanceof IErlModule) {
            return;
        }

        if (element instanceof ISourceReference) {
            final ISourceReference reference = (ISourceReference) element;
            // set highlight range
            setSelection(reference, true);
            if (myOutlinePage != null) {
                myOutlinePage.select(reference);
            }
        }
    }

    private IWorkbenchPart getActivePart() {
        final IWorkbenchWindow window = getSite().getWorkbenchWindow();
        final IPartService service = window.getPartService();
        final IWorkbenchPart part = service.getActivePart();
        return part;
    }

    @Override
    public void createPartControl(final Composite parent) {
        try {
            super.createPartControl(parent);
        } catch (final IllegalArgumentException e) {
            // #661: nicer message if file is encoded in utf-8
            throw filterUTF8Exception(e);
        }

        setupBracketInserter();

        final ProjectionViewer v = (ProjectionViewer) getSourceViewer();
        v.doOperation(ProjectionViewer.TOGGLE);

        fEditorSelectionChangedListener = new EditorSelectionChangedListener();
        fEditorSelectionChangedListener.install(getSelectionProvider());

        final IEclipsePreferences node = ErlideUIPlugin.getPrefsNode();
        node.addPreferenceChangeListener(fPreferenceChangeListener);
        ErlideUIPlugin.getDefault().getPreferenceStore()
                .addPropertyChangeListener(propertyChangeListener);

        PlatformUI.getWorkbench().addWindowListener(
                markOccurencesHandler.fActivationListener);

        if (markOccurencesHandler.isMarkingOccurrences()) {
            markOccurencesHandler.installOccurrencesFinder(true);
        }
    }

    private IllegalArgumentException filterUTF8Exception(final IllegalArgumentException e) {
        final StackTraceElement[] stack = e.getStackTrace();
        boolean filterIt = false;
        for (final StackTraceElement element : stack) {
            if (filterIt) {
                break;
            }
            if (!element.getClassName().equals("org.eclipse.swt.SWT")) {
                filterIt = element.getClassName().equals(
                        "org.eclipse.swt.custom.StyledText")
                        && element.getMethodName().equals("setStyleRanges");
            }
        }
        if (filterIt) {
            return new IllegalArgumentException(
                    "The file's actual encoding doesn't match the declared one", e);
        }
        return e;
    }

    protected boolean isActivePart() {
        final IWorkbenchPart part = getActivePart();
        return part != null && part.equals(this);
    }

    public final ISourceViewer getViewer() {
        return getSourceViewer();
    }

    @Override
    public ViewerComparator createDefaultOutlineComparator() {
        return null;
    }

    @Override
    public ViewerComparator createOutlineComparator() {
        return null;
    }

    @Override
    public ITreeContentProvider createOutlineContentProvider() {
        return new ErlangContentProvider();
    }

    @Override
    public ILabelProvider createOutlineLabelProvider() {
        final ErlangLabelProvider erlangLabelProvider = new ErlangLabelProvider();
        erlangLabelProvider.addLabelDecorator(new ProblemsLabelDecorator());
        return erlangLabelProvider;
    }

    @Override
    public Object getOutlineInput() {
        return getModule();
    }

    @Override
    public ISortableContentOutlinePage getContentOutline() {
        return myOutlinePage;
    }

    @Override
    public void updateSelection(final SelectionChangedEvent event) {
        final ISelection sel = event.getSelection();
        if (sel instanceof IStructuredSelection) {
            final IStructuredSelection structuredSelection = (IStructuredSelection) sel;
            updateSelection(structuredSelection.getFirstElement());
        }
    }

    @Override
    public void updateSelection(final Object sel) {
    }

    public void selectionChanged(final SelectionChangedEvent event) {
        if (event.getSource() == getSelectionProvider()) {
            return;
        }
        final ISelection sel = event.getSelection();
        if (sel instanceof ITextSelection) {
            return;
        }
        if (sel instanceof IStructuredSelection) {
            fSelection = ((IStructuredSelection) sel).getFirstElement();
        } else {
            fSelection = null;
        }
    }

    public Object getSelection() {
        return fSelection;
    }

    @Override
    public void doSave(final IProgressMonitor progressMonitor) {
        // TODO: maybe this should be in a resource change listener?
        super.doSave(progressMonitor);
        resetAndCacheScannerAndParser();
    }

    public void resetAndCacheScannerAndParser() {
        final IErlModule module = getModule();
        if (module == null) {
            return;
        }
        resetReconciler();
        try {
            module.createScanner();
            module.getScanner().dispose();
            module.resetAndCacheScannerAndParser(getDocument().get());
        } catch (final ErlModelException e) {
            ErlLogger.error(e);
        }
    }

    public void resetReconciler() {
        ((EditorConfiguration) getSourceViewerConfiguration()).resetReconciler();
    }

    @Override
    public void reconcileNow() {
        ((EditorConfiguration) getSourceViewerConfiguration()).reconcileNow();
    }

    public ActionGroup getActionGroup() {
        return fActionGroups;
    }

    /**
     * Runner that will toggle folding either instantly (if the editor is
     * visible) or the next time it becomes visible. If a runner is started when
     * there is already one registered, the registered one is canceled as
     * toggling folding twice is a no-op.
     * <p>
     * The access to the fFoldingRunner field is not thread-safe, it is assumed
     * that <code>runWhenNextVisible</code> is only called from the UI thread.
     * </p>
     *
     * @since 3.1
     */
    final class ToggleFoldingRunner implements IPartListener2 {
        /**
         * The workbench page we registered the part listener with, or
         * <code>null</code>.
         */
        private IWorkbenchPage fPage;

        /**
         * Does the actual toggling of projection.
         */
        @SuppressWarnings("synthetic-access")
        private void toggleFolding() {
            final ISourceViewer sourceViewer = getSourceViewer();
            if (sourceViewer instanceof ProjectionViewer) {
                final ProjectionViewer pv = (ProjectionViewer) sourceViewer;
                if (pv.isProjectionMode() != EditorUtility.isFoldingEnabled()) {
                    if (pv.canDoOperation(ProjectionViewer.TOGGLE)) {
                        pv.doOperation(ProjectionViewer.TOGGLE);
                    }
                }
            }
        }

        /**
         * Makes sure that the editor's folding state is correct the next time
         * it becomes visible. If it already is visible, it toggles the folding
         * state. If not, it either registers a part listener to toggle folding
         * when the editor becomes visible, or cancels an already registered
         * runner.
         */
        public void runWhenNextVisible() {
            // if there is one already: toggling twice is the identity
            if (fFoldingRunner != null) {
                fFoldingRunner.cancel();
                return;
            }
            final IWorkbenchPartSite site = getSite();
            if (site != null) {
                final IWorkbenchPage page = site.getPage();
                if (!page.isPartVisible(ErlangEditor.this)) {
                    // if we're not visible - defer until visible
                    fPage = page;
                    fFoldingRunner = this;
                    page.addPartListener(this);
                    return;
                }
            }
            // we're visible - run now
            toggleFolding();
            fFoldingRunner = null;
        }

        /**
         * Remove the listener and clear the field.
         */
        private void cancel() {
            if (fPage != null) {
                fPage.removePartListener(this);
                fPage = null;
            }
            if (fFoldingRunner == this) {
                fFoldingRunner = null;
            }
        }

        /*
         * @see org.eclipse.ui.IPartListener2#partVisible(org.eclipse.ui.
         * IWorkbenchPartReference)
         */
        @Override
        public void partVisible(final IWorkbenchPartReference partRef) {
            if (ErlangEditor.this.equals(partRef.getPart(false))) {
                cancel();
                toggleFolding();
                fFoldingRunner = null;
            }
        }

        /*
         * @seeorg.eclipse.ui.IPartListener2#partClosed(org.eclipse.ui.
         * IWorkbenchPartReference)
         */
        @Override
        public void partClosed(final IWorkbenchPartReference partRef) {
            if (ErlangEditor.this.equals(partRef.getPart(false))) {
                cancel();
            }
        }

        @Override
        public void partActivated(final IWorkbenchPartReference partRef) {
        }

        @Override
        public void partBroughtToTop(final IWorkbenchPartReference partRef) {
        }

        @Override
        public void partDeactivated(final IWorkbenchPartReference partRef) {
        }

        @Override
        public void partOpened(final IWorkbenchPartReference partRef) {
        }

        @Override
        public void partHidden(final IWorkbenchPartReference partRef) {
        }

        @Override
        public void partInputChanged(final IWorkbenchPartReference partRef) {
        }
    }

    @Override
    protected void handlePreferenceStoreChanged(final PropertyChangeEvent event) {
        final String property = event.getProperty();
        final ISourceViewer sourceViewer = getSourceViewer();
        try {
            if (PreferenceConstants.EDITOR_FOLDING_ENABLED.equals(property)) {
                if (sourceViewer instanceof ProjectionViewer) {
                    new ToggleFoldingRunner().runWhenNextVisible();
                }
                return;
            }
        } finally {
            super.handlePreferenceStoreChanged(event);
        }
    }

    public void expandCollapseFunctionsOrComments(final boolean collapse,
            final boolean comments) {
        if (fProjectionModelUpdater instanceof IErlangFoldingStructureProviderExtension) {
            final IErlangFoldingStructureProviderExtension ext = (IErlangFoldingStructureProviderExtension) fProjectionModelUpdater;
            if (collapse) {
                if (comments) {
                    ext.collapseComments();
                } else {
                    ext.collapseFunctions();
                }
            } else {
                ext.expandAll();
            }
        }
    }

    /**
     * Returns the lock object for the given annotation model.
     *
     * @param annotationModel
     *            the annotation model
     * @return the annotation model's lock object
     * @since 3.0
     */
    Object getLockObject(final IAnnotationModel annotationModel) {
        if (annotationModel instanceof ISynchronizable) {
            final Object lock = ((ISynchronizable) annotationModel).getLockObject();
            if (lock != null) {
                return lock;
            }
        }
        return annotationModel;
    }

    @Override
    protected ScannerService getNewScanner() {
        return getModule().getScanner();
    }

    /**
     * Mutex for the reconciler. See
     * https://bugs.eclipse.org/bugs/show_bug.cgi?id=63898 for a description of
     * the problem.
     * <p>
     * remove once the underlying problem
     * (https://bugs.eclipse.org/bugs/show_bug.cgi?id=66176) is solved.
     * </p>
     */
    private final Object fReconcilerLock = new Object();

    /**
     * Returns the mutex for the reconciler. See
     * https://bugs.eclipse.org/bugs/show_bug.cgi?id=63898 for a description of
     * the problem.
     * <p>
     * remove once the underlying problem
     * (https://bugs.eclipse.org/bugs/show_bug.cgi?id=66176) is solved.
     * </p>
     *
     * @return the lock reconcilers may use to synchronize on
     */
    public Object getReconcilerLock() {
        return fReconcilerLock;
    }

    @Override
    public IErlProject getProject() {
        return ErlangEngine.getInstance().getModelUtilService().getProject(getModule());
    }

    @Override
    public String getScannerName() {
        return getModule().getScannerName();
    }

    /**
     * Returns whether the given annotation is configured as a target for the
     * "Go to Next/Previous Annotation" actions
     *
     * @param annotation
     *            the annotation
     * @return <code>true</code> if this is a target, <code>false</code>
     *         otherwise
     * @since 3.0
     */
    @Override
    protected boolean isNavigationTarget(final Annotation annotation) {
        return annotationSupport.isNavigationTarget(annotation);
    }

    /**
     * Returns whether the given annotation is configured as a target for the
     * "Go to Next/Previous Annotation" actions
     *
     * @param annotation
     *            the annotation
     * @return <code>true</code> if this is a target, <code>false</code>
     *         otherwise
     * @since 3.0
     */
    @Override
    public Annotation gotoAnnotation(final boolean forward) {
        return annotationSupport.gotoAnnotation(forward);
    }
}
