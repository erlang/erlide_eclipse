package org.erlide.ui.editors.scratchpad;

import java.util.ResourceBundle;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.ITextHoverExtension2;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.ITextViewerExtension;
import org.eclipse.jface.text.ITextViewerExtension2;
import org.eclipse.jface.text.ITextViewerExtension4;
import org.eclipse.jface.text.ITextViewerExtension5;
import org.eclipse.jface.text.TextUtilities;
import org.eclipse.jface.text.information.IInformationProvider;
import org.eclipse.jface.text.information.IInformationProviderExtension;
import org.eclipse.jface.text.information.IInformationProviderExtension2;
import org.eclipse.jface.text.information.InformationPresenter;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.jface.text.source.projection.ProjectionSupport;
import org.eclipse.jface.text.source.projection.ProjectionViewer;
import org.eclipse.search.ui.IContextMenuConstants;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.ISaveablePart2;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionContext;
import org.eclipse.ui.actions.ActionGroup;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;
import org.eclipse.ui.texteditor.ChainedPreferenceStore;
import org.eclipse.ui.texteditor.ContentAssistAction;
import org.eclipse.ui.texteditor.ITextEditorActionDefinitionIds;
import org.eclipse.ui.texteditor.ResourceAction;
import org.eclipse.ui.texteditor.TextEditorAction;
import org.eclipse.ui.texteditor.TextOperationAction;
import org.erlide.model.erlang.ErlToken;
import org.erlide.model.erlang.IErlModule;
import org.erlide.model.erlang.IErlScanner;
import org.erlide.model.root.ErlModelManager;
import org.erlide.model.root.IErlElement;
import org.erlide.model.root.IErlProject;
import org.erlide.ui.actions.CompositeActionGroup;
import org.erlide.ui.actions.ErlangSearchActionGroup;
import org.erlide.ui.actions.OpenAction;
import org.erlide.ui.editors.erl.AbstractErlangEditor;
import org.erlide.ui.editors.erl.ColorManager;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.erl.ErlangEditorMessages;
import org.erlide.ui.editors.erl.ErlangSourceViewerConfiguration;
import org.erlide.ui.editors.erl.IErlangEditorActionDefinitionIds;
import org.erlide.ui.editors.erl.IErlangHelpContextIds;
import org.erlide.ui.editors.erl.actions.IndentAction;
import org.erlide.ui.editors.erl.actions.SendToConsoleAction;
import org.erlide.ui.editors.erl.actions.ToggleCommentAction;
import org.erlide.ui.editors.erl.autoedit.SmartTypingPreferencePage;
import org.erlide.ui.editors.erl.folding.IErlangFoldingStructureProvider;
import org.erlide.ui.editors.erl.scanner.IErlangPartitions;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.utils.ErlLogger;
import org.erlide.utils.Util;

public class ErlangScratchPad extends AbstractErlangEditor implements
        ISaveablePart2 {

    private ColorManager colorManager;
    private final IPreferenceChangeListener fPreferenceChangeListener = new PreferenceChangeListener();
    private InformationPresenter fInformationPresenter;
    private ProjectionSupport fProjectionSupport;
    private IErlangFoldingStructureProvider fProjectionModelUpdater;
    private CompositeActionGroup fActionGroups;
    private CompositeActionGroup fContextMenuGroup;
    private OpenAction openAction;
    private SendToConsoleAction sendToConsole;
    private IndentAction indentAction;
    private ToggleCommentAction toggleCommentAction;
    private IErlScanner erlScanner = null;

    /**
     * Simple constructor
     * 
     */
    public ErlangScratchPad() {
        super();
        registerListeners();
    }

    // FIXME copied from ErlangEditor
    class PreferenceChangeListener implements IPreferenceChangeListener {
        @Override
        public void preferenceChange(final PreferenceChangeEvent event) {
            final String key = event.getKey();
            // ErlLogger.debug("event:: " + key);
            if (key.indexOf('/') != -1
                    && key.split("/")[0]
                            .equals(SmartTypingPreferencePage.SMART_TYPING_KEY)) {
                ErlangEditor.readBracketInserterPrefs(getBracketInserter());
            }
        }
    }

    private void registerListeners() {
    }

    /**
     * Simple disposer
     * 
     * @see org.eclipse.ui.IWorkbenchPart#dispose()
     */
    @Override
    public void dispose() {
        if (colorManager != null) {
            colorManager.dispose();
            colorManager = null;
        }

        final ISourceViewer sourceViewer = getSourceViewer();
        if (sourceViewer instanceof ITextViewerExtension) {
            ((ITextViewerExtension) sourceViewer)
                    .removeVerifyKeyListener(getBracketInserter());
        }
        final IEclipsePreferences node = ErlideUIPlugin.getPrefsNode();
        node.removePreferenceChangeListener(fPreferenceChangeListener);

        if (fActionGroups != null) {
            fActionGroups.dispose();
            fActionGroups = null;
        }
        if (fProjectionModelUpdater != null) {
            fProjectionModelUpdater.uninstall();
        }

        super.dispose();
    }

    @Override
    protected void initializeEditor() {
        colorManager = new ColorManager();
        setDocumentProvider(new TextFileDocumentProvider());

        final IPreferenceStore store = getErlangEditorPreferenceStore();
        setPreferenceStore(store);

        final ErlangSourceViewerConfiguration cfg = new ErlangScratchPadConfiguration(
                getPreferenceStore(), colorManager, this);
        setSourceViewerConfiguration(cfg);
    }

    @Override
    public IErlProject getProject() {
        final IFile file = getFile();
        if (file != null) {
            final IProject project = file.getProject();
            if (project != null) {
                return ErlModelManager.getErlangModel().findProject(project);
            }
        }
        return null;
    }

    public static ChainedPreferenceStore getErlangEditorPreferenceStore() {
        final IPreferenceStore generalTextStore = EditorsUI
                .getPreferenceStore();
        return new ChainedPreferenceStore(new IPreferenceStore[] {
                ErlideUIPlugin.getDefault().getPreferenceStore(),
                generalTextStore });
    }

    @Override
    public void createPartControl(final Composite parent) {
        super.createPartControl(parent);

        ErlangEditor.readBracketInserterPrefs(getBracketInserter());

        final ISourceViewer sourceViewer = getSourceViewer();
        if (sourceViewer instanceof ITextViewerExtension) {
            ((ITextViewerExtension) sourceViewer)
                    .prependVerifyKeyListener(getBracketInserter());
        }

        final ProjectionViewer v = (ProjectionViewer) getSourceViewer();
        v.doOperation(ProjectionViewer.TOGGLE);

        final IEclipsePreferences node = ErlideUIPlugin.getPrefsNode();
        node.addPreferenceChangeListener(fPreferenceChangeListener);

        final IInformationControlCreator informationControlCreator = getSourceViewerConfiguration()
                .getInformationControlCreator(getSourceViewer());

        fInformationPresenter = new InformationPresenter(
                informationControlCreator);
        // sizes: see org.eclipse.jface.text.TextViewer.TEXT_HOVER_*_CHARS
        fInformationPresenter.setSizeConstraints(100, 12, true, true);
        fInformationPresenter.install(getSourceViewer());
        fInformationPresenter
                .setDocumentPartitioning(getSourceViewerConfiguration()
                        .getConfiguredDocumentPartitioning(getSourceViewer()));
    }

    @Override
    protected ISourceViewer createSourceViewer(final Composite parent,
            final IVerticalRuler ruler, final int styles) {
        final ISourceViewer viewer = new ProjectionViewer(parent, ruler,
                getOverviewRuler(), true, styles);
        getSourceViewerDecorationSupport(viewer);

        /*
         * This is a performance optimization to reduce the computation of the
         * text presentation triggered by {@link #setVisibleDocument(IDocument)}
         */
        // if (javaSourceViewer != null && isFoldingEnabled() && (store == null
        // ||
        // !store.getBoolean(PreferenceConstants.EDITOR_SHOW_SEGMENTS)))
        // javaSourceViewer.prepareDelayedProjection();
        if (ErlangEditor.isFoldingEnabled()) {
            final ProjectionViewer projectionViewer = (ProjectionViewer) viewer;
            fProjectionSupport = new ProjectionSupport(projectionViewer,
                    getAnnotationAccess(), getSharedColors());
            fProjectionSupport
                    .addSummarizableAnnotationType("org.eclipse.ui.workbench.texteditor.error"); //$NON-NLS-1$
            fProjectionSupport
                    .addSummarizableAnnotationType("org.eclipse.ui.workbench.texteditor.warning"); //$NON-NLS-1$
            // TODO fProjectionSupport.setHoverControlCreator(new
            // IInformationControlCreator()
            // {
            // public IInformationControl createInformationControl(Shell shell)
            // {
            // return new CustomSourceInformationControl(shell,
            // IDocument.DEFAULT_CONTENT_TYPE);
            // }
            // });

            fProjectionSupport.install();

            fProjectionModelUpdater = ErlideUIPlugin.getDefault()
                    .getFoldingStructureProviderRegistry()
                    .getCurrentFoldingProvider();
            if (fProjectionModelUpdater != null) {
                fProjectionModelUpdater.install(this, projectionViewer);
            }
        }
        return viewer;
    }

    @Override
    protected void createActions() {
        super.createActions();
        // ActionGroup oeg, ovg, jsg;
        ActionGroup esg;
        fActionGroups = new CompositeActionGroup(new ActionGroup[] {
        // oeg= new OpenEditorActionGroup(this),
        // ovg= new OpenViewActionGroup(this),
        esg = new ErlangSearchActionGroup(this) });
        fContextMenuGroup = new CompositeActionGroup(new ActionGroup[] { esg });

        openAction = new OpenAction(this);
        openAction
                .setActionDefinitionId(IErlangEditorActionDefinitionIds.OPEN_EDITOR);
        setAction(IErlangEditorActionDefinitionIds.OPEN, openAction);

        sendToConsole = new SendToConsoleAction(getSite(),
                ErlangEditorMessages.getBundleForConstructedKeys(),
                "SendToConsole.", this);
        sendToConsole
                .setActionDefinitionId(IErlangEditorActionDefinitionIds.SEND_TO_CONSOLE);
        setAction("SendToConsole", sendToConsole);
        markAsStateDependentAction("sendToConsole", true);
        markAsSelectionDependentAction("sendToConsole", true);

        final Action act = new ContentAssistAction(
                ErlangEditorMessages.getBundleForConstructedKeys(),
                "ContentAssistProposal.", this);
        act.setActionDefinitionId(ITextEditorActionDefinitionIds.CONTENT_ASSIST_PROPOSALS);
        setAction("ContentAssistProposal", act);
        markAsStateDependentAction("ContentAssistProposal", true);

        ResourceAction resAction = new TextOperationAction(
                ErlangEditorMessages.getBundleForConstructedKeys(),
                "ShowEDoc.", this, ISourceViewer.INFORMATION, true); //$NON-NLS-1$
        resAction = new InformationDispatchAction(
                ErlangEditorMessages.getBundleForConstructedKeys(),
                "ShowEDoc.", (TextOperationAction) resAction); //$NON-NLS-1$
        resAction
                .setActionDefinitionId(IErlangEditorActionDefinitionIds.SHOW_EDOC);
        setAction("ShowEDoc", resAction); //$NON-NLS-1$
        PlatformUI.getWorkbench().getHelpSystem()
                .setHelp(resAction, IErlangHelpContextIds.SHOW_EDOC_ACTION);

        indentAction = new IndentAction(
                ErlangEditorMessages.getBundleForConstructedKeys(),
                "Indent.", this); //$NON-NLS-1$
        indentAction
                .setActionDefinitionId(IErlangEditorActionDefinitionIds.INDENT);
        setAction("Indent", indentAction); //$NON-NLS-1$
        markAsStateDependentAction("Indent", true); //$NON-NLS-1$
        markAsSelectionDependentAction("Indent", true); //$NON-NLS-1$
        PlatformUI.getWorkbench().getHelpSystem()
                .setHelp(indentAction, IErlangHelpContextIds.INDENT_ACTION);

        // if (ErlideUtil.isTest()) {
        // testAction = new TestAction(ErlangEditorMessages
        // .getBundleForConstructedKeys(), "Test.", this, getModule());
        // testAction
        // .setActionDefinitionId(IErlangEditorActionDefinitionIds.TEST);
        // setAction("Test", testAction);
        // markAsStateDependentAction("Test", true);
        // markAsSelectionDependentAction("Test", true);
        // // PlatformUI.getWorkbench().getHelpSystem().setHelp(indentAction,
        // // IErlangHelpContextIds.INDENT_ACTION);
        // }

        final Action action = new IndentAction(
                ErlangEditorMessages.getBundleForConstructedKeys(), "Indent.",
                this);
        setAction("IndentOnTab", action);
        markAsStateDependentAction("IndentOnTab", true);
        markAsSelectionDependentAction("IndentOnTab", true);

        toggleCommentAction = new ToggleCommentAction(
                ErlangEditorMessages.getBundleForConstructedKeys(),
                "ToggleComment.", this);
        toggleCommentAction
                .setActionDefinitionId(IErlangEditorActionDefinitionIds.TOGGLE_COMMENT);
        setAction("ToggleComment", toggleCommentAction);
        markAsStateDependentAction("ToggleComment", true);
        markAsSelectionDependentAction("ToggleComment", true);
        PlatformUI
                .getWorkbench()
                .getHelpSystem()
                .setHelp(toggleCommentAction,
                        IErlangHelpContextIds.TOGGLE_COMMENT_ACTION);
    }

    // FIXME Copied from ErlangEditor
    /**
     * This action behaves in two different ways: If there is no current text
     * hover, the javadoc is displayed using information presenter. If there is
     * a current text hover, it is converted into a information presenter in
     * order to make it sticky.
     */
    class InformationDispatchAction extends TextEditorAction {

        /** The wrapped text operation action. */
        private final TextOperationAction fTextOperationAction;

        /**
         * Creates a dispatch action.
         * 
         * @param resourceBundle
         *            the resource bundle
         * @param prefix
         *            the prefix
         * @param textOperationAction
         *            the text operation action
         */
        public InformationDispatchAction(final ResourceBundle resourceBundle,
                final String prefix,
                final TextOperationAction textOperationAction) {
            super(resourceBundle, prefix, ErlangScratchPad.this);
            if (textOperationAction == null) {
                throw new IllegalArgumentException();
            }
            fTextOperationAction = textOperationAction;
        }

        /*
         * @see org.eclipse.jface.action.IAction#run()
         */
        @SuppressWarnings("synthetic-access")
        @Override
        public void run() {

            /**
             * Information provider used to present the information.
             * 
             * @since 3.0
             */
            class InformationProvider implements IInformationProvider,
                    IInformationProviderExtension,
                    IInformationProviderExtension2 {

                private final IRegion fHoverRegion;

                private final String fHoverInfo;

                private final IInformationControlCreator fControlCreator;

                InformationProvider(final IRegion hoverRegion,
                        final String hoverInfo,
                        final IInformationControlCreator controlCreator) {
                    fHoverRegion = hoverRegion;
                    fHoverInfo = hoverInfo;
                    fControlCreator = controlCreator;
                }

                /*
                 * @seeorg.eclipse.jface.text.information.IInformationProvider#
                 * getSubject(org.eclipse.jface.text.ITextViewer, int)
                 */

                @Override
                public IRegion getSubject(final ITextViewer textViewer,
                        final int invocationOffset) {
                    return fHoverRegion;
                }

                @Override
                public Object getInformation2(final ITextViewer textViewer,
                        final IRegion subject) {
                    return fHoverInfo;
                }

                /*
                 * @see
                 * org.eclipse.jface.text.information.IInformationProviderExtension2
                 * #getInformationPresenterControlCreator()
                 * 
                 * @since 3.0
                 */

                @Override
                public IInformationControlCreator getInformationPresenterControlCreator() {
                    return fControlCreator;
                }

                @Override
                @Deprecated
                public String getInformation(final ITextViewer textViewer,
                        final IRegion subject) {
                    return null;
                }
            }

            final ISourceViewer sourceViewer = getSourceViewer();
            if (sourceViewer == null) {
                fTextOperationAction.run();
                return;
            }

            if (sourceViewer instanceof ITextViewerExtension4) {
                final ITextViewerExtension4 extension4 = (ITextViewerExtension4) sourceViewer;
                if (extension4.moveFocusToWidgetToken()) {
                    return;
                }
            }

            if (!(sourceViewer instanceof ITextViewerExtension2)) {
                fTextOperationAction.run();
                return;
            }

            final ITextViewerExtension2 textViewerExtension2 = (ITextViewerExtension2) sourceViewer;

            // does a text hover exist?
            final ITextHover textHover = textViewerExtension2
                    .getCurrentTextHover();
            if (textHover == null) {
                // TODO this crashes...
                // fTextOperationAction.run();
                return;
            }

            final Point hoverEventLocation = textViewerExtension2
                    .getHoverEventLocation();
            final int offset = computeOffsetAtLocation(sourceViewer,
                    hoverEventLocation.x, hoverEventLocation.y);
            if (offset == -1) {
                fTextOperationAction.run();
                return;
            }

            try {
                // get the text hover content
                final String contentType = TextUtilities.getContentType(
                        sourceViewer.getDocument(),
                        IErlangPartitions.ERLANG_PARTITIONING, offset, true);

                final IRegion hoverRegion = textHover.getHoverRegion(
                        sourceViewer, offset);
                if (hoverRegion == null) {
                    return;
                }

                final String hoverInfo = "";
                if (textHover instanceof ITextHoverExtension2) {
                    ((ITextHoverExtension2) textHover).getHoverInfo2(
                            sourceViewer, hoverRegion);
                }

                IInformationControlCreator controlCreator = null;
                if (textHover instanceof IInformationProviderExtension2) {
                    controlCreator = ((IInformationProviderExtension2) textHover)
                            .getInformationPresenterControlCreator();
                }

                final IInformationProvider informationProvider = new InformationProvider(
                        hoverRegion, hoverInfo, controlCreator);

                fInformationPresenter.setOffset(offset);
                fInformationPresenter
                        .setDocumentPartitioning(IErlangPartitions.ERLANG_PARTITIONING);
                fInformationPresenter.setInformationProvider(
                        informationProvider, contentType);
                fInformationPresenter.showInformation();
            } catch (final BadLocationException e) {
            }
        }

        // modified version from TextViewer
        private int computeOffsetAtLocation(final ITextViewer textViewer,
                final int x, final int y) {

            final StyledText styledText = textViewer.getTextWidget();
            final IDocument document = textViewer.getDocument();

            if (document == null) {
                return -1;
            }

            try {
                final int widgetLocation = styledText
                        .getOffsetAtLocation(new Point(x, y));
                if (textViewer instanceof ITextViewerExtension5) {
                    final ITextViewerExtension5 extension = (ITextViewerExtension5) textViewer;
                    return extension.widgetOffset2ModelOffset(widgetLocation);
                }
                final IRegion visibleRegion = textViewer.getVisibleRegion();
                return widgetLocation + visibleRegion.getOffset();
            } catch (final IllegalArgumentException e) {
                return -1;
            }

        }
    }

    @Override
    protected void editorContextMenuAboutToShow(final IMenuManager menu) {
        super.editorContextMenuAboutToShow(menu);

        // if (ErlideUtil.isTest()) {
        // menu.prependToGroup(IContextMenuConstants.GROUP_OPEN, testAction);
        // }
        menu.prependToGroup(IContextMenuConstants.GROUP_OPEN,
                toggleCommentAction);
        menu.prependToGroup(IContextMenuConstants.GROUP_OPEN, indentAction);
        menu.prependToGroup(IContextMenuConstants.GROUP_OPEN, openAction);
        menu.prependToGroup(IContextMenuConstants.GROUP_OPEN, sendToConsole);
        final ActionContext context = new ActionContext(getSelectionProvider()
                .getSelection());
        fContextMenuGroup.setContext(context);
        fContextMenuGroup.fillContextMenu(menu);
        fContextMenuGroup.setContext(null);
    }

    // Auto-saving when quitting or closing, through ISaveablePart2

    @Override
    public int promptToSaveOnClose() {
        doSave(getProgressMonitor());
        return NO;
    }

    @Override
    protected void initializeKeyBindingScopes() {
        setKeyBindingScopes(new String[] { "org.erlide.ui.erlangEditorScope" }); //$NON-NLS-1$
    }

    @Override
    public void reconcileNow() {
        // TODO Auto-generated method stub

    }

    @Override
    public IErlElement getElementAt(final int offset, final boolean b) {
        return null;
    }

    @Override
    public IErlModule getModule() {
        return null;
    }

    @Override
    public IDocument getDocument() {
        return getDocumentProvider().getDocument(this);
    }

    @Override
    public ErlToken getTokenAt(final int offset) {
        return getScanner().getTokenAt(offset);
    }

    private IFile getFile() {
        final IEditorInput editorInput = getEditorInput();
        if (editorInput instanceof IFileEditorInput) {
            final IFileEditorInput input = (IFileEditorInput) editorInput;
            return input.getFile();
        }
        return null;
    }

    @Override
    public IErlScanner getScanner() {
        if (erlScanner == null) {
            final IFile file = getFile();
            if (file != null) {
                try {
                    final String filePath = file.getLocation()
                            .toPortableString();
                    String initialText;
                    initialText = Util.getInputStreamAsString(
                            file.getContents(), file.getCharset());
                    erlScanner = ErlModelManager
                            .getErlangModel()
                            .getToolkit()
                            .createScanner(getScannerName(), initialText,
                                    filePath, false);
                } catch (final CoreException e) {
                    ErlLogger.warn(e);
                }
            }
        }
        return erlScanner;
    }

    @Override
    public String getScannerName() {
        final IFile file = getFile();
        if (file != null) {
            final IPath fullPath = file.getFullPath();
            final String scannerName = "scratchPad"
                    + fullPath.toPortableString().hashCode() + "_"
                    + fullPath.removeFileExtension().lastSegment();
            return scannerName;
        }
        return null;
    }

}
