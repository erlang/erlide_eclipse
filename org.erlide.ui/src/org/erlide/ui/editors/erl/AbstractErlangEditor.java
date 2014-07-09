package org.erlide.ui.editors.erl;

import java.util.ResourceBundle;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.ITextViewerExtension;
import org.eclipse.jface.text.information.InformationPresenter;
import org.eclipse.jface.text.source.ICharacterPairMatcher;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.texteditor.ContentAssistAction;
import org.eclipse.ui.texteditor.ITextEditorActionConstants;
import org.eclipse.ui.texteditor.ITextEditorActionDefinitionIds;
import org.eclipse.ui.texteditor.ITextEditorExtension3;
import org.eclipse.ui.texteditor.SourceViewerDecorationSupport;
import org.eclipse.ui.texteditor.TextOperationAction;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.parsing.ScannerService;
import org.erlide.ui.actions.OpenAction;
import org.erlide.ui.editors.erl.actions.SendToConsoleAction;
import org.erlide.ui.prefs.PreferenceConstants;

public abstract class AbstractErlangEditor extends TextEditor {

    protected final static String MATCHING_BRACKETS = PreferenceConstants.EDITOR_MATCHING_BRACKETS;
    protected final static String MATCHING_BRACKETS_COLOR = PreferenceConstants.EDITOR_MATCHING_BRACKETS_COLOR;
    private ErlangBracketInserter fBracketInserter;
    private SendToConsoleAction sendToConsole;
    private SendToConsoleAction sendToConsoleWithResult;
    private OpenAction openAction;
    InformationPresenter fInformationPresenter;
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

    protected ErlangBracketInserter getBracketInserter() {
        if (fBracketInserter == null) {
            fBracketInserter = new ErlangBracketInserter(getSourceViewer());
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
                getOverviewRuler(), true, styles, new IBracketInserterValidator() {
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
        openAction = new OpenAction(this);
        openAction.setActionDefinitionId(IErlangEditorActionDefinitionIds.OPEN_EDITOR);
        setAction(IErlangEditorActionDefinitionIds.OPEN, openAction);

        final ResourceBundle keyBundle = ErlangEditorMessages
                .getBundleForConstructedKeys();
        sendToConsole = new SendToConsoleAction(getSite(), keyBundle, "SendToConsole.",
                this, false, getProject());
        sendToConsole
                .setActionDefinitionId(IErlangEditorActionDefinitionIds.SEND_TO_CONSOLE);
        setAction("SendToConsole", sendToConsole);
        markAsStateDependentAction("sendToConsole", true);
        markAsSelectionDependentAction("sendToConsole", true);

        sendToConsoleWithResult = new SendToConsoleAction(getSite(), keyBundle,
                "SendToConsoleWithResult.", this, true, getProject());
        sendToConsoleWithResult
                .setActionDefinitionId(IErlangEditorActionDefinitionIds.SEND_TO_CONSOLE_WITH_RESULT);
        setAction("SendToConsoleWithResult", sendToConsoleWithResult);
        markAsStateDependentAction("sendToConsoleWithResult", true);
        markAsSelectionDependentAction("sendToConsoleWithResult", true);

        final Action contentAssistAction = new ContentAssistAction(keyBundle,
                "ContentAssistProposal.", this);
        contentAssistAction
                .setActionDefinitionId(ITextEditorActionDefinitionIds.CONTENT_ASSIST_PROPOSALS);
        setAction("ContentAssistProposal", contentAssistAction);
        markAsStateDependentAction("ContentAssistProposal", true);

        final TextOperationAction showEdocAction0 = new TextOperationAction(keyBundle,
                "ShowEDoc.", this, ISourceViewer.INFORMATION, true);
        final ShowEDocAction showEdocAction = new ShowEDocAction(this, getSourceViewer(),
                keyBundle, "ShowEDoc.", showEdocAction0);
        showEdocAction.setActionDefinitionId(IErlangEditorActionDefinitionIds.SHOW_EDOC);
        setAction("ShowEDoc", showEdocAction);
        PlatformUI.getWorkbench().getHelpSystem()
                .setHelp(showEdocAction, IErlangHelpContextIds.SHOW_EDOC_ACTION);
    }

    protected void addCommonActions(final IMenuManager menu) {
        // TODO disabled until erl_tidy doesn't destroy formatting
        // menu.prependToGroup(IContextMenuConstants.GROUP_OPEN, cleanUpAction);
        menu.prependToGroup(ITextEditorActionConstants.GROUP_OPEN, openAction);
        menu.appendToGroup(ITextEditorActionConstants.GROUP_REST, sendToConsole);
        menu.appendToGroup(ITextEditorActionConstants.GROUP_REST, sendToConsoleWithResult);
    }

    @Override
    public void createPartControl(final Composite parent) {
        super.createPartControl(parent);

        final IInformationControlCreator informationControlCreator = getSourceViewerConfiguration()
                .getInformationControlCreator(getSourceViewer());
        fInformationPresenter = new InformationPresenter(informationControlCreator);
        // sizes: see org.eclipse.jface.text.TextViewer.TEXT_HOVER_*_CHARS
        fInformationPresenter.setSizeConstraints(100, 12, true, true);
        fInformationPresenter.install(getSourceViewer());
        fInformationPresenter.setDocumentPartitioning(getSourceViewerConfiguration()
                .getConfiguredDocumentPartitioning(getSourceViewer()));
    }

}
