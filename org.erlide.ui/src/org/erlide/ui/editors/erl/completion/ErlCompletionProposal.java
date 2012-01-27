package org.erlide.ui.editors.erl.completion;

import java.util.List;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.link.ILinkedModeListener;
import org.eclipse.jface.text.link.LinkedModeModel;
import org.eclipse.jface.text.link.LinkedModeUI;
import org.eclipse.jface.text.link.LinkedPosition;
import org.eclipse.jface.text.link.LinkedPositionGroup;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.ui.texteditor.link.EditorLinkedModeUI;
import org.erlide.jinterface.ErlLogger;

public class ErlCompletionProposal implements ICompletionProposal {

    protected static final class ExitPolicy implements LinkedModeUI.IExitPolicy {

        final char fExitCharacter;
        private final IDocument fDocument;

        public ExitPolicy(final char exitCharacter, final IDocument document) {
            fExitCharacter = exitCharacter;
            fDocument = document;
        }

        /*
         * @see
         * org.eclipse.jdt.internal.ui.text.link.LinkedPositionUI.ExitPolicy
         * #doExit(org.eclipse.jdt.internal.ui.text.link.LinkedPositionManager,
         * org.eclipse.swt.events.VerifyEvent, int, int)
         */
        @Override
        public LinkedModeUI.ExitFlags doExit(final LinkedModeModel environment,
                final VerifyEvent event, final int offset, final int length) {

            if (event.character == fExitCharacter) {
                if (environment.anyPositionContains(offset)) {
                    return new LinkedModeUI.ExitFlags(
                            ILinkedModeListener.UPDATE_CARET, false);
                }
                return new LinkedModeUI.ExitFlags(
                        ILinkedModeListener.UPDATE_CARET, true);
            }

            switch (event.character) {
            case ';':
                return new LinkedModeUI.ExitFlags(ILinkedModeListener.NONE,
                        true);
            case SWT.CR:
                // FIXME this is Java specific!!!
                // // when entering an anonymous class as a parameter, we don't
                // // want
                // // to jump after the parenthesis when return is pressed
                // if (offset > 0) {
                // try {
                // if (fDocument.getChar(offset - 1) == '{') {
                // return new LinkedModeUI.ExitFlags(
                // ILinkedModeListener.EXIT_ALL, true);
                // }
                // } catch (final BadLocationException e) {
                // }
                // }
                return null;
            default:
                return null;
            }
        }

    }

    /** List of offsets (x) and lengths (y) for linked mode replacements */
    private final List<Point> offsetsAndLengths;
    /** The string to be displayed in the completion proposal popup. */
    private final String displayString;
    /** The replacement string. */
    private final String replacementString;
    /** The replacement offset. */
    private final int replacementOffset;
    /** The replacement length. */
    private final int replacementLength;
    /** The cursor position after this proposal has been applied. */
    private final int cursorPosition;
    /** The image to be displayed in the completion proposal popup. */
    private final Image image;
    /** The context information of this proposal. */
    private final IContextInformation contextInformation;
    /** The additional info of this proposal. */
    private final String additionalProposalInfo;
    /** A sourceViewer (from the erlang editor) */
    private final ISourceViewer sourceViewer;

    /**
     * @param offsetsAndLengths
     * @param displayString
     * @param replacementString
     * @param replacementOffset
     * @param replacementLength
     * @param cursorPosition
     * @param image
     * @param contextInformation
     * @param additionalProposalInfo
     * @param sourceViewer
     */
    public ErlCompletionProposal(final List<Point> offsetsAndLengths,
            final String displayString, final String replacementString,
            final int replacementOffset, final int replacementLength,
            final int cursorPosition, final Image image,
            final IContextInformation contextInformation,
            final String additionalProposalInfo,
            final ISourceViewer sourceViewer) {
        this.offsetsAndLengths = offsetsAndLengths;
        this.displayString = displayString;
        this.replacementString = replacementString;
        this.replacementOffset = replacementOffset;
        this.replacementLength = replacementLength;
        this.cursorPosition = cursorPosition;
        this.image = image;
        this.contextInformation = contextInformation;
        this.additionalProposalInfo = additionalProposalInfo;
        this.sourceViewer = sourceViewer;
    }

    /*
     * @see ICompletionProposal#apply(IDocument)
     */
    @Override
    public void apply(final IDocument document) {
        try {
            document.replace(replacementOffset, replacementLength,
                    replacementString);
            setUpLinkedMode(document, ')', offsetsAndLengths);
        } catch (final BadLocationException x) {
            // ignore
        }
    }

    /*
     * @see ICompletionProposal#getAdditionalProposalInfo()
     */
    @Override
    public String getAdditionalProposalInfo() {
        return additionalProposalInfo;
    }

    /*
     * @see ICompletionProposal#getContextInformation()
     */
    @Override
    public IContextInformation getContextInformation() {
        return contextInformation;
    }

    /*
     * @see ICompletionProposal#getDisplayString()
     */
    @Override
    public String getDisplayString() {
        if (displayString != null) {
            return displayString;
        }
        return replacementString;
    }

    /*
     * @see ICompletionProposal#getImage()
     */
    @Override
    public Image getImage() {
        return image;
    }

    /*
     * @see ICompletionProposal#getSelection(IDocument)
     */
    @Override
    public Point getSelection(final IDocument document) {
        if (offsetsAndLengths.isEmpty()) {
            return new Point(replacementOffset + cursorPosition, 0);
        }
        return offsetsAndLengths.get(0);
    }

    /**
     * Sets up a simple linked mode at {@link #getCursorPosition()} and an exit
     * policy that will exit the mode when <code>closingCharacter</code> is
     * typed and an exit position at <code>getCursorPosition() + 1</code>.
     * 
     * @param document
     *            the document
     * @param closingCharacter
     *            the exit character
     * @param offsetsAndLengths
     *            list of offsets and lengths for linked groups
     */
    protected void setUpLinkedMode(final IDocument document,
            final char closingCharacter, final List<Point> offsetsAndLengths) {
        if (sourceViewer != null && !offsetsAndLengths.isEmpty()) {
            try {
                final LinkedModeModel model = new LinkedModeModel();
                int last = 0, i = 0;
                for (final Point offsetAndLength : offsetsAndLengths) {
                    final LinkedPositionGroup group = new LinkedPositionGroup();
                    group.addPosition(new LinkedPosition(document,
                            offsetAndLength.x, offsetAndLength.y, ++i));
                    model.addGroup(group);
                    final int l = offsetAndLength.x + offsetAndLength.y;
                    if (l > last) {
                        last = l;
                    }
                }
                model.forceInstall();

                final LinkedModeUI ui = new EditorLinkedModeUI(model,
                        sourceViewer);
                // ui.setSimpleMode(true);
                ui.setExitPolicy(new ExitPolicy(closingCharacter, document));
                ui.setExitPosition(sourceViewer, last + 1, 0,
                        LinkedPositionGroup.NO_STOP);
                ui.setCyclingMode(LinkedModeUI.CYCLE_ALWAYS);
                ui.enter();
            } catch (final BadLocationException x) {
                ErlLogger.error(x);
            }
        }
    }
}
