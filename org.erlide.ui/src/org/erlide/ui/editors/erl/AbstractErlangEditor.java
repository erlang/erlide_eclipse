package org.erlide.ui.editors.erl;

import java.util.ResourceBundle;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
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
import org.eclipse.jface.text.source.ICharacterPairMatcher;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.search.ui.IContextMenuConstants;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.texteditor.ContentAssistAction;
import org.eclipse.ui.texteditor.ITextEditorActionDefinitionIds;
import org.eclipse.ui.texteditor.ITextEditorExtension3;
import org.eclipse.ui.texteditor.ResourceAction;
import org.eclipse.ui.texteditor.SourceViewerDecorationSupport;
import org.eclipse.ui.texteditor.TextEditorAction;
import org.eclipse.ui.texteditor.TextOperationAction;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.parsing.ScannerService;
import org.erlide.ui.actions.OpenAction;
import org.erlide.ui.editors.erl.actions.IndentAction;
import org.erlide.ui.editors.erl.actions.SendToConsoleAction;
import org.erlide.ui.editors.erl.actions.ToggleCommentAction;
import org.erlide.ui.editors.erl.scanner.IErlangPartitions;
import org.erlide.ui.prefs.PreferenceConstants;

public abstract class AbstractErlangEditor extends TextEditor {

    /** Preference key for matching brackets */
    protected final static String MATCHING_BRACKETS = PreferenceConstants.EDITOR_MATCHING_BRACKETS;
    /** Preference key for matching brackets color */
    protected final static String MATCHING_BRACKETS_COLOR = PreferenceConstants.EDITOR_MATCHING_BRACKETS_COLOR;
    /** The bracket inserter. */
    private ErlangViewerBracketInserter fBracketInserter = null;
    private SendToConsoleAction sendToConsole;
    private SendToConsoleAction sendToConsoleWithResult;
    private OpenAction openAction;
    private IndentAction indentAction;
    private ToggleCommentAction toggleCommentAction;
    private InformationPresenter fInformationPresenter;
    private ScannerService erlScanner;

    public abstract void reconcileNow();

    public abstract IErlElement getElementAt(int offset, boolean b);

    public abstract IErlModule getModule();

    public ScannerService getScanner() {
        if (erlScanner == null) {
            erlScanner = getNewScanner();
        }
        return erlScanner;
    }

    protected abstract ScannerService getNewScanner();

    @Override
    protected void configureSourceViewerDecorationSupport(
            final SourceViewerDecorationSupport support) {
        support.setCharacterPairMatcher(getBracketMatcher());
        support.setMatchingCharacterPainterPreferenceKeys(MATCHING_BRACKETS,
                MATCHING_BRACKETS_COLOR);

        super.configureSourceViewerDecorationSupport(support);
    }

    public ICharacterPairMatcher getBracketMatcher() {
        return ((ErlangSourceViewerConfiguration) getSourceViewerConfiguration())
                .getBracketMatcher();
    }

    protected ErlangViewerBracketInserter getBracketInserter() {
        if (fBracketInserter == null) {
            fBracketInserter = new ErlangViewerBracketInserter(
                    getSourceViewer());
        }
        return fBracketInserter;
    }

    public abstract IErlProject getProject();

    public abstract String getScannerName();

    protected void setupBracketInserter() {
        final ISourceViewer sourceViewer = getSourceViewer();
        if (sourceViewer instanceof ITextViewerExtension) {
            ((ITextViewerExtension) sourceViewer)
                    .prependVerifyKeyListener(getBracketInserter());
        }
    }

    @Override
    protected ISourceViewer createSourceViewer(final Composite parent,
            final IVerticalRuler ruler, final int styles) {
        final ISourceViewer viewer = new ErlangSourceViewer(parent, ruler,
                getOverviewRuler(), true, styles,
                new IBracketInserterValidator() {
                    @Override
                    public boolean earlyCancelCheck() {
                        return getInsertMode() != ITextEditorExtension3.SMART_INSERT;
                    }

                    @Override
                    public boolean validInput() {
                        return validateEditorInputState();
                    }
                });
        getSourceViewerDecorationSupport(viewer);

        addFoldingSupport(viewer);

        return viewer;
    }

    protected abstract void addFoldingSupport(final ISourceViewer viewer);

    protected void createCommonActions() {
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

        openAction = new OpenAction(this);
        openAction
                .setActionDefinitionId(IErlangEditorActionDefinitionIds.OPEN_EDITOR);
        setAction(IErlangEditorActionDefinitionIds.OPEN, openAction);

        sendToConsole = new SendToConsoleAction(getSite(),
                ErlangEditorMessages.getBundleForConstructedKeys(),
                "SendToConsole.", this, false, getProject());
        sendToConsole
                .setActionDefinitionId(IErlangEditorActionDefinitionIds.SEND_TO_CONSOLE);
        setAction("SendToConsole", sendToConsole);
        markAsStateDependentAction("sendToConsole", true);
        markAsSelectionDependentAction("sendToConsole", true);

        sendToConsoleWithResult = new SendToConsoleAction(getSite(),
                ErlangEditorMessages.getBundleForConstructedKeys(),
                "SendToConsoleWithResult.", this, true, getProject());
        sendToConsoleWithResult
                .setActionDefinitionId(IErlangEditorActionDefinitionIds.SEND_TO_CONSOLE_WITH_RESULT);
        setAction("SendToConsoleWithResult", sendToConsoleWithResult);
        markAsStateDependentAction("sendToConsoleWithResult", true);
        markAsSelectionDependentAction("sendToConsoleWithResult", true);

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
    }

    protected void addCommonActions(final IMenuManager menu) {
        menu.prependToGroup(IContextMenuConstants.GROUP_OPEN,
                toggleCommentAction);
        menu.prependToGroup(IContextMenuConstants.GROUP_OPEN, indentAction);
        // TODO disabled until erl_tidy doean't destroy formatting
        // menu.prependToGroup(IContextMenuConstants.GROUP_OPEN, cleanUpAction);
        menu.prependToGroup(IContextMenuConstants.GROUP_OPEN, openAction);
        menu.prependToGroup(IContextMenuConstants.GROUP_OPEN, sendToConsole);
        menu.prependToGroup(IContextMenuConstants.GROUP_OPEN,
                sendToConsoleWithResult);
    }

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
            super(resourceBundle, prefix, AbstractErlangEditor.this);
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
                // TODO this crashes... why?
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
    public void createPartControl(final Composite parent) {
        super.createPartControl(parent);

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

}
