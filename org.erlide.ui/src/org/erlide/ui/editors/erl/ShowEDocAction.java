package org.erlide.ui.editors.erl;

import java.util.ResourceBundle;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.ITextHoverExtension2;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.ITextViewerExtension2;
import org.eclipse.jface.text.ITextViewerExtension4;
import org.eclipse.jface.text.ITextViewerExtension5;
import org.eclipse.jface.text.TextUtilities;
import org.eclipse.jface.text.information.IInformationProvider;
import org.eclipse.jface.text.information.IInformationProviderExtension;
import org.eclipse.jface.text.information.IInformationProviderExtension2;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Point;
import org.eclipse.ui.texteditor.TextEditorAction;
import org.eclipse.ui.texteditor.TextOperationAction;
import org.erlide.ui.editors.erl.scanner.IErlangPartitions;

/**
 * This action behaves in two different ways: If there is no current text hover,
 * the javadoc is displayed using information presenter. If there is a current
 * text hover, it is converted into a information presenter in order to make it
 * sticky.
 */
class ShowEDocAction extends TextEditorAction {

    /** The wrapped text operation action. */
    private final TextOperationAction fTextOperationAction;
    private final AbstractErlangEditor editor;
    final ISourceViewer sourceViewer;

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
    public ShowEDocAction(final AbstractErlangEditor editor,
            final ISourceViewer sourceViewer, final ResourceBundle resourceBundle,
            final String prefix, final TextOperationAction textOperationAction) {
        super(resourceBundle, prefix, editor);
        if (textOperationAction == null) {
            throw new IllegalArgumentException();
        }
        this.editor = editor;
        this.sourceViewer = sourceViewer;
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
                IInformationProviderExtension, IInformationProviderExtension2 {

            private final IRegion fHoverRegion;

            private final String fHoverInfo;

            private final IInformationControlCreator fControlCreator;

            InformationProvider(final IRegion hoverRegion, final String hoverInfo,
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
        final ITextHover textHover = textViewerExtension2.getCurrentTextHover();
        if (textHover == null) {
            // TODO this crashes... why?
            // fTextOperationAction.run();
            return;
        }

        final Point hoverEventLocation = textViewerExtension2.getHoverEventLocation();
        final int offset = computeOffsetAtLocation(sourceViewer, hoverEventLocation.x,
                hoverEventLocation.y);
        if (offset == -1) {
            fTextOperationAction.run();
            return;
        }

        try {
            // get the text hover content
            final String contentType = TextUtilities.getContentType(
                    sourceViewer.getDocument(), IErlangPartitions.ERLANG_PARTITIONING,
                    offset, true);

            final IRegion hoverRegion = textHover.getHoverRegion(sourceViewer, offset);
            if (hoverRegion == null) {
                return;
            }

            final String hoverInfo = "";
            if (textHover instanceof ITextHoverExtension2) {
                ((ITextHoverExtension2) textHover).getHoverInfo2(sourceViewer,
                        hoverRegion);
            }

            IInformationControlCreator controlCreator = null;
            if (textHover instanceof IInformationProviderExtension2) {
                controlCreator = ((IInformationProviderExtension2) textHover)
                        .getInformationPresenterControlCreator();
            }

            final IInformationProvider informationProvider = new InformationProvider(
                    hoverRegion, hoverInfo, controlCreator);

            editor.fInformationPresenter.setOffset(offset);
            editor.fInformationPresenter
                    .setDocumentPartitioning(IErlangPartitions.ERLANG_PARTITIONING);
            editor.fInformationPresenter.setInformationProvider(informationProvider,
                    contentType);
            editor.fInformationPresenter.showInformation();
        } catch (final BadLocationException e) {
        }
    }

    // modified version from TextViewer
    private int computeOffsetAtLocation(final ITextViewer textViewer, final int x,
            final int y) {

        final StyledText styledText = textViewer.getTextWidget();
        final IDocument document = textViewer.getDocument();

        if (document == null) {
            return -1;
        }

        try {
            final int widgetLocation = styledText.getOffsetAtLocation(new Point(x, y));
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
