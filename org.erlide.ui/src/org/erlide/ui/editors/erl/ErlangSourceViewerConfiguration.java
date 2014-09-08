package org.erlide.ui.editors.erl;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.IAutoEditStrategy;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.ITextDoubleClickStrategy;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.quickassist.IQuickAssistAssistant;
import org.eclipse.jface.text.quickassist.QuickAssistAssistant;
import org.eclipse.jface.text.reconciler.IReconciler;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.ITokenScanner;
import org.eclipse.jface.text.source.ICharacterPairMatcher;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.editors.text.TextSourceViewerConfiguration;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditorPreferenceConstants;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.ui.editors.erl.completion.ErlContentAssistProcessor;
import org.erlide.ui.editors.erl.completion.ErlStringContentAssistProcessor;
import org.erlide.ui.editors.erl.correction.ErlangQuickAssistProcessor;
import org.erlide.ui.editors.erl.hover.ErlTextHover;
import org.erlide.ui.editors.erl.scanner.ErlCodeScanner;
import org.erlide.ui.editors.erl.scanner.ErlCommentScanner;
import org.erlide.ui.editors.erl.scanner.ErlDamagerRepairer;
import org.erlide.ui.editors.erl.scanner.ErlStringScanner;
import org.erlide.ui.editors.erl.scanner.ErlTokenScanner;
import org.erlide.ui.editors.erl.scanner.IErlangPartitions;
import org.erlide.ui.editors.erl.scanner.SingleTokenScanner;
import org.erlide.ui.editors.internal.reconciling.ErlReconciler;
import org.erlide.ui.editors.internal.reconciling.ErlReconcilingStrategy;
import org.erlide.ui.internal.information.ErlInformationPresenter;
import org.erlide.ui.prefs.TokenHighlight;
import org.erlide.ui.prefs.plugin.ColoringPreferencePage;
import org.erlide.ui.util.IColorManager;
import org.erlide.util.IDisposable;

public class ErlangSourceViewerConfiguration extends TextSourceViewerConfiguration
        implements IDisposable {

    protected final IColorManager colorManager;
    protected ErlTokenScanner charScanner;
    protected ErlTokenScanner codeScanner;
    protected final ErlTokenScanner commentScanner;
    protected final ErlTokenScanner stringScanner;
    protected final ErlTokenScanner qatomScanner;
    private ICharacterPairMatcher fBracketMatcher;
    private ITextDoubleClickStrategy doubleClickStrategy;
    private ErlContentAssistProcessor contentAssistProcessor;
    private ErlStringContentAssistProcessor contentAssistProcessorForStrings;

    public ErlangSourceViewerConfiguration(final IPreferenceStore store,
            final IColorManager colorManager) {
        super(store);
        this.colorManager = colorManager;
        codeScanner = new ErlCodeScanner(colorManager);

        commentScanner = new ErlCommentScanner(colorManager);
        stringScanner = new ErlStringScanner(colorManager);
        qatomScanner = new SingleTokenScanner(colorManager,
                ErlTokenScanner.getToken(TokenHighlight.ATOM.getName()));
        charScanner = new SingleTokenScanner(colorManager,
                ErlTokenScanner.getToken(TokenHighlight.CHAR.getName()));
    }

    @Override
    public String[] getConfiguredContentTypes(final ISourceViewer sourceViewer) {
        return IErlangPartitions.LEGAL_PARTITIONS;
    }

    @Override
    public String getConfiguredDocumentPartitioning(final ISourceViewer sourceViewer) {
        return IErlangPartitions.ERLANG_PARTITIONING;
    }

    @Override
    public IPresentationReconciler getPresentationReconciler(
            final ISourceViewer sourceViewer) {
        final PresentationReconciler reconciler = new ErlangPresentationReconciler();
        reconciler
                .setDocumentPartitioning(getConfiguredDocumentPartitioning(sourceViewer));
        DefaultDamagerRepairer dr = new ErlDamagerRepairer(codeScanner);

        final ITokenScanner scan2 = new ErlCodeScanner(colorManager);
        dr = new ErlDamagerRepairer(scan2);
        reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE);
        reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE);

        dr = new ErlDamagerRepairer(commentScanner);
        reconciler.setDamager(dr, IErlangPartitions.ERLANG_COMMENT);
        reconciler.setRepairer(dr, IErlangPartitions.ERLANG_COMMENT);
        dr = new ErlDamagerRepairer(stringScanner);
        reconciler.setDamager(dr, IErlangPartitions.ERLANG_STRING);
        reconciler.setRepairer(dr, IErlangPartitions.ERLANG_STRING);
        dr = new ErlDamagerRepairer(qatomScanner);
        reconciler.setDamager(dr, IErlangPartitions.ERLANG_QATOM);
        reconciler.setRepairer(dr, IErlangPartitions.ERLANG_QATOM);
        dr = new ErlDamagerRepairer(charScanner);
        reconciler.setDamager(dr, IErlangPartitions.ERLANG_CHARACTER);
        reconciler.setRepairer(dr, IErlangPartitions.ERLANG_CHARACTER);

        return reconciler;
    }

    public ICharacterPairMatcher getBracketMatcher() {
        if (fBracketMatcher == null) {
            fBracketMatcher = new ErlangPairMatcher(new String[] { "(", ")", "{", "}",
                    "[", "]", "<<", ">>" });
        }
        return fBracketMatcher;
    }

    public boolean affectsTextPresentation(final PropertyChangeEvent event) {
        return event.getProperty().startsWith(ColoringPreferencePage.COLORS_QUALIFIER);
    }

    public void handlePropertyChangeEvent(final PropertyChangeEvent event) {
        String id = null;
        RGB color = null;
        int style = -1;

        final String property = event.getProperty();
        final Object newValue = event.getNewValue();
        if (TokenHighlight.isColorKey(property)) {
            id = TokenHighlight.getKeyName(property);
            try {
                color = newValue != null ? StringConverter.asRGB((String) newValue)
                        : null;
            } catch (final Exception e) {
                color = null;
            }
        } else if (TokenHighlight.isStylesKey(property)) {
            id = TokenHighlight.getKeyName(property);
            if (newValue instanceof Integer) {
                style = (Integer) newValue;
            } else if (newValue instanceof String) {
                try {
                    style = Integer.parseInt((String) newValue);
                } catch (final Exception e) {
                    style = -1;
                }
            } else {
                style = -1;
            }
        }
        if (id != null) {
            codeScanner.handleColorChange(id, color, style);
            commentScanner.handleColorChange(id, color, style);
            stringScanner.handleColorChange(id, color, style);
            charScanner.handleColorChange(id, color, style);
            qatomScanner.handleColorChange(id, color, style);
        }
    }

    @Override
    public ITextDoubleClickStrategy getDoubleClickStrategy(
            final ISourceViewer sourceViewer, final String contentType) {
        if (doubleClickStrategy == null) {
            // doubleClickStrategy = new
            // ErlDoubleClickSelector(getBracketMatcher());
            doubleClickStrategy = new DoubleClickStrategy(getBracketMatcher());
        }
        return doubleClickStrategy;
    }

    @Override
    public IInformationControlCreator getInformationControlCreator(
            final ISourceViewer sourceViewer) {
        return new IInformationControlCreator() {

            @Override
            public IInformationControl createInformationControl(final Shell parent) {
                return new DefaultInformationControl(parent,
                        EditorsUI.getTooltipAffordanceString(),
                        new ErlInformationPresenter(true));
            }
        };
    }

    @Override
    public IQuickAssistAssistant getQuickAssistAssistant(final ISourceViewer sourceViewer) {
        final IQuickAssistAssistant assistant = new QuickAssistAssistant();
        assistant.setQuickAssistProcessor(new ErlangQuickAssistProcessor());
        assistant
                .setInformationControlCreator(getQuickAssistAssistantInformationControlCreator());
        return assistant;
    }

    private IInformationControlCreator getQuickAssistAssistantInformationControlCreator() {
        return new IInformationControlCreator() {
            @Override
            public IInformationControl createInformationControl(final Shell parent) {
                final String affordance = getAdditionalInfoAffordanceString();
                return new DefaultInformationControl(parent, affordance);
            }
        };
    }

    static final String getAdditionalInfoAffordanceString() {
        if (!EditorsUI
                .getPreferenceStore()
                .getBoolean(
                        AbstractDecoratedTextEditorPreferenceConstants.EDITOR_SHOW_TEXT_HOVER_AFFORDANCE)) {
            return null;
        }

        return "Press 'Tab' from proposal table or click for focus";
    }

    protected IErlProject getProject() {
        return null;
    }

    protected IErlModule getModule() {
        return null;
    }

    @Override
    public IContentAssistant getContentAssistant(final ISourceViewer sourceViewer) {
        final ContentAssistant contentAssistant = new ContentAssistant();
        contentAssistant
                .setDocumentPartitioning(getConfiguredDocumentPartitioning(sourceViewer));

        final IErlModule module = getModule();
        final IErlProject project = getProject();
        contentAssistProcessor = new ErlContentAssistProcessor(sourceViewer, module,
                project, contentAssistant);
        contentAssistProcessorForStrings = new ErlStringContentAssistProcessor(
                sourceViewer, module, project, contentAssistant);

        contentAssistProcessor.setToPrefs();
        contentAssistant.setContentAssistProcessor(contentAssistProcessor,
                IDocument.DEFAULT_CONTENT_TYPE);
        contentAssistant.setContentAssistProcessor(contentAssistProcessor,
                IErlangPartitions.ERLANG_QATOM);
        contentAssistant.setContentAssistProcessor(contentAssistProcessorForStrings,
                IErlangPartitions.ERLANG_STRING);
        contentAssistant.enableAutoInsert(true);
        contentAssistant.enablePrefixCompletion(false);
        contentAssistant.setDocumentPartitioning(IErlangPartitions.ERLANG_PARTITIONING);

        contentAssistant.setProposalPopupOrientation(IContentAssistant.PROPOSAL_OVERLAY);
        contentAssistant
                .setContextInformationPopupOrientation(IContentAssistant.CONTEXT_INFO_ABOVE);
        contentAssistant
                .setInformationControlCreator(getInformationControlCreator(sourceViewer));

        return contentAssistant;
    }

    @Override
    public ITextHover getTextHover(final ISourceViewer sourceViewer,
            final String contentType) {
        return new ErlTextHover(null);
    }

    protected final static IAutoEditStrategy[] NO_AUTOEDIT = new IAutoEditStrategy[] {};

    @Override
    public IAutoEditStrategy[] getAutoEditStrategies(final ISourceViewer sourceViewer,
            final String contentType) {
        return NO_AUTOEDIT;
    }

    @Override
    public IReconciler getReconciler(final ISourceViewer sourceViewer) {
        final ErlReconcilingStrategy strategy = new ErlReconcilingStrategy(null);
        final IErlModule module = null;
        final String path = null;
        final ErlReconciler reconciler = new ErlReconciler(strategy, true, true, path,
                module, null);
        reconciler.setProgressMonitor(new NullProgressMonitor());
        reconciler.setIsAllowedToModifyDocument(false);
        reconciler.setDelay(500);
        return reconciler;
    }

    @Override
    public String[] getDefaultPrefixes(final ISourceViewer sourceViewer,
            final String contentType) {
        return new String[] { "%%", "" };
    }

    @Override
    public void dispose() {
        if (contentAssistProcessor != null) {
            contentAssistProcessor.dispose();
            contentAssistProcessor = null;
            contentAssistProcessorForStrings = null;
        }
    }
}
