package org.erlide.ui.editors.erl;

import java.util.List;
import java.util.Stack;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.BadPositionCategoryException;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentExtension;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.jface.text.IPositionUpdater;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.link.ILinkedModeListener;
import org.eclipse.jface.text.link.LinkedModeModel;
import org.eclipse.jface.text.link.LinkedModeUI;
import org.eclipse.jface.text.link.LinkedModeUI.ExitFlags;
import org.eclipse.jface.text.link.LinkedModeUI.IExitPolicy;
import org.eclipse.jface.text.link.LinkedPosition;
import org.eclipse.jface.text.link.LinkedPositionGroup;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.VerifyKeyListener;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.ui.texteditor.ITextEditorExtension3;
import org.eclipse.ui.texteditor.link.EditorLinkedModeUI;
import org.erlide.backend.BackendException;
import org.erlide.core.internal.model.erlang.ErlideScanner;
import org.erlide.core.model.erlang.ErlToken;
import org.erlide.jinterface.ErlLogger;

class ErlangEditorBracketInserter implements VerifyKeyListener,
        ILinkedModeListener {

    private final ErlangEditor fErlangEditor;
    private final ISourceViewer fSourceViewer;

    public static class BracketLevel {
        int fOffset;
        int fLength;
        LinkedModeUI fUI;
        Position fFirstPosition;
        Position fSecondPosition;
    }

    private static class ExclusivePositionUpdater implements IPositionUpdater {

        /** The position category. */
        private final String fCategory;

        /**
         * Creates a new updater for the given <code>category</code>.
         * 
         * @param category
         *            the new category.
         */
        public ExclusivePositionUpdater(final String category) {
            fCategory = category;
        }

        /*
         * @see
         * org.eclipse.jface.text.IPositionUpdater#update(org.eclipse.jface.
         * text.DocumentEvent)
         */
        @Override
        public void update(final DocumentEvent event) {

            final int eventOffset = event.getOffset();
            final int eventOldLength = event.getLength();
            final int eventNewLength = event.getText() == null ? 0 : event
                    .getText().length();
            final int deltaLength = eventNewLength - eventOldLength;

            try {
                final Position[] positions = event.getDocument().getPositions(
                        fCategory);

                for (int i = 0; i != positions.length; i++) {

                    final Position position = positions[i];

                    if (position.isDeleted()) {
                        continue;
                    }

                    final int offset = position.getOffset();
                    final int length = position.getLength();
                    final int end = offset + length;

                    if (offset >= eventOffset + eventOldLength) {
                        // position comes after change - shift
                        position.setOffset(offset + deltaLength);
                    } else if (end <= eventOffset) {
                        // position comes way before change - leave alone
                    } else if (offset <= eventOffset
                            && end >= eventOffset + eventOldLength) {
                        // event completely internal to the position - adjust
                        // length
                        position.setLength(length + deltaLength);
                    } else if (offset < eventOffset) {
                        // event extends over end of position - adjust length
                        final int newEnd = eventOffset;
                        position.setLength(newEnd - offset);
                    } else if (end > eventOffset + eventOldLength) {
                        // event extends from before position into it - adjust
                        // offset and length offset becomes end of event, length
                        // adjusted accordingly
                        final int newOffset = eventOffset + eventNewLength;
                        position.setOffset(newOffset);
                        position.setLength(end - newOffset);
                    } else {
                        // event consumes the position - delete it
                        position.delete();
                    }
                }
            } catch (final BadPositionCategoryException e) {
                // ignore and return
            }
        }
    }

    private boolean fCloseBraces = false;
    private boolean fCloseBrackets = false;
    private boolean fCloseStrings = false;
    private boolean fCloseParens = false;
    private boolean fCloseAtoms = false;
    private boolean fEmbraceSelection = true;

    private final String CATEGORY = toString();
    private final IPositionUpdater fUpdater = new ExclusivePositionUpdater(
            CATEGORY);
    private final Stack<BracketLevel> fBracketLevelStack = new Stack<BracketLevel>();

    public void setCloseBracketsEnabled(final boolean enabled) {
        fCloseBrackets = enabled;
    }

    public void setCloseAtomsEnabled(final boolean enabled) {
        fCloseAtoms = enabled;
    }

    public void setCloseParensEnabled(final boolean enabled) {
        fCloseParens = enabled;
    }

    public void setCloseBracesEnabled(final boolean enabled) {
        fCloseBraces = enabled;
    }

    public void setCloseStringsEnabled(final boolean enabled) {
        fCloseStrings = enabled;
    }

    public void setEmbraceSelectionEnabled(final boolean enabled) {
        fEmbraceSelection = enabled;
    }

    // private boolean isStopper(final String kind) {
    // return kind.equals("(") || kind.equals(")") || kind.equals("{")
    // || kind.equals("}") || kind.equals("[") || kind.equals("]")
    // || kind.equals("'") || kind.equals("\"") || kind.equals("atom");
    // }

    /*
     * @see org.eclipse.swt.custom.VerifyKeyListener#verifyKey(org.eclipse.swt
     * .events.VerifyEvent)
     */
    @Override
    public void verifyKey(final VerifyEvent event) {

        // early pruning to slow down normal typing as little as possible
        if (!event.doit
                || fErlangEditor.getInsertMode() != ITextEditorExtension3.SMART_INSERT) {
            return;
        }
        switch (event.character) {
        case '(':
        case '{':
        case '[':
        case '\'':
        case '\"':
            break;
        default:
            return;
        }

        final IDocument document = fSourceViewer.getDocument();

        final Point selection = fSourceViewer.getSelectedRange();
        final int offset = selection.x;
        final int length = selection.y;
        try {
            final String selStr = fEmbraceSelection ? document.get(offset,
                    length) : "";
            if (selStr.length() == 0) {
                final int kind = getKindOfBracket(document, offset, length);
                // if (isStopper(kind)) {
                // return;
                // }
                if (kind == '(' || kind == '{' || kind == '[') {
                    return;
                }

                switch (event.character) {
                case '(':
                    if (!fCloseParens || kind == ')') {
                        return;
                    }
                    break;

                case '[':
                    if (!fCloseBrackets || kind == ']') {
                        return;
                    }
                    break;
                case '{':
                    if (!fCloseBraces || kind == '}') {
                        return;
                    }
                    break;
                case '\'':
                    if (!fCloseAtoms || kind == '\'') {
                        return;
                    }
                    break;
                case '"':
                    if (!fCloseStrings || kind == '"') {
                        return;
                    }
                    break;

                default:
                    return;
                }

                if (!fErlangEditor.validateEditorInputState()) {
                    return;
                }
            }
            final char character = event.character;
            final char closingCharacter = getPeerCharacter(character);
            updateDocument(document, offset, length, selStr, character,
                    closingCharacter);

            event.doit = false;

        } catch (final BadLocationException e) {
            ErlLogger.error(e);
        } catch (final BadPositionCategoryException e) {
            ErlLogger.error(e);
        }
    }

    private void updateDocumentSelection(final IDocument document,
            final int offset, final String selStr, final char closingCharacter)
            throws BadLocationException, BadPositionCategoryException {
        final BracketLevel level = new BracketLevel();
        fBracketLevelStack.push(level);

        final LinkedPositionGroup group = new LinkedPositionGroup();
        group.addPosition(new LinkedPosition(document, offset + 1, 0,
                LinkedPositionGroup.NO_STOP));

        final LinkedModeModel model = new LinkedModeModel();
        model.addLinkingListener(this);
        model.addGroup(group);
        model.forceInstall();

        level.fOffset = offset;
        level.fLength = 2 + selStr.length();

        // set up position tracking for our magic peers
        if (fBracketLevelStack.size() == 1) {
            document.addPositionCategory(CATEGORY);
            document.addPositionUpdater(fUpdater);
        }
        level.fFirstPosition = new Position(offset, 1);
        level.fSecondPosition = new Position(offset + 1, 1);
        document.addPosition(CATEGORY, level.fFirstPosition);
        document.addPosition(CATEGORY, level.fSecondPosition);

        level.fUI = new EditorLinkedModeUI(model, fSourceViewer);
        level.fUI.setSimpleMode(true);
        level.fUI.setExitPolicy(new ExitPolicy(closingCharacter,
                getEscapeCharacter(closingCharacter), fBracketLevelStack));
        level.fUI.setExitPosition(fSourceViewer, offset + 2 + selStr.length(),
                0, Integer.MAX_VALUE);
        level.fUI.setCyclingMode(LinkedModeUI.CYCLE_NEVER);
        level.fUI.enter();

        final IRegion newSelection = level.fUI.getSelectedRegion();
        fSourceViewer.setSelectedRange(newSelection.getOffset(),
                newSelection.getLength());
    }

    private void updateDocument(final IDocument document, final int offset,
            final int length, final String selStr, final char character,
            final char closingCharacter) throws BadLocationException,
            BadPositionCategoryException {
        final StringBuilder buffer = new StringBuilder();
        buffer.append(character);
        buffer.append(selStr);
        buffer.append(closingCharacter);

        document.replace(offset, length, buffer.toString());

        updateDocumentSelection(document, offset, selStr, closingCharacter);
    }

    private int getKindOfBracket(final IDocument document, final int offset,
            final int length) throws BadLocationException {
        final IRegion endLine = document.getLineInformationOfOffset(offset
                + length);

        List<ErlToken> tokens = null;
        final int getOffset = offset + length, getLength = endLine.getOffset()
                + endLine.getLength() - getOffset;
        final String str = document.get(getOffset, getLength);
        try {
            tokens = ErlideScanner.lightScanString(str, 0);
        } catch (final BackendException e) {
        }

        int kind = ErlToken.KIND_OTHER;
        if (tokens != null && tokens.size() > 0) {
            kind = tokens.get(0).getKind();
        } else if (str.length() > 0) {
            kind = str.charAt(0);
        }
        return kind;
    }

    /*
     * @see org.eclipse.jface.text.link.ILinkedModeListener#left(org.eclipse.
     * jface.text.link.LinkedModeModel, int)
     */
    @Override
    @SuppressWarnings("synthetic-access")
    public void left(final LinkedModeModel environment, final int flags) {

        final BracketLevel level = fBracketLevelStack.pop();

        if (flags != ILinkedModeListener.EXTERNAL_MODIFICATION) {
            return;
        }

        // remove brackets
        final ISourceViewer sourceViewer = fSourceViewer;
        final IDocument document = sourceViewer.getDocument();
        if (document instanceof IDocumentExtension) {
            final IDocumentExtension extension = (IDocumentExtension) document;
            extension.registerPostNotificationReplace(null,
                    new IDocumentExtension.IReplace() {

                        @Override
                        public void perform(final IDocument d,
                                final IDocumentListener owner) {
                            if ((level.fFirstPosition.isDeleted || level.fFirstPosition.length == 0)
                                    && !level.fSecondPosition.isDeleted
                                    && level.fSecondPosition.offset == level.fFirstPosition.offset) {
                                try {
                                    document.replace(
                                            level.fSecondPosition.offset,
                                            level.fSecondPosition.length, ""); //$NON-NLS-1$
                                } catch (final BadLocationException e) {
                                    ErlLogger.error(e);
                                }
                            }

                            if (fBracketLevelStack.size() == 0) {
                                document.removePositionUpdater(fUpdater);
                                try {
                                    document.removePositionCategory(CATEGORY);
                                } catch (final BadPositionCategoryException e) {
                                    ErlLogger.error(e);
                                }
                            }
                        }
                    });
        }

    }

    /*
     * @see org.eclipse.jface.text.link.ILinkedModeListener#suspend(org.eclipse
     * .jface.text.link.LinkedModeModel)
     */
    @Override
    public void suspend(final LinkedModeModel environment) {
    }

    /*
     * @see org.eclipse.jface.text.link.ILinkedModeListener#resume(org.eclipse
     * .jface.text.link.LinkedModeModel, int)
     */
    @Override
    public void resume(final LinkedModeModel environment, final int flags) {
    }

    private static char getPeerCharacter(final char character) {
        switch (character) {
        case '(':
            return ')';

        case ')':
            return '(';

        case '{':
            return '}';

        case '}':
            return '{';

        case '[':
            return ']';

        case ']':
            return '[';

        case '"':
            return character;

        case '\'':
            return character;

        default:
            throw new IllegalArgumentException();
        }
    }

    static char getEscapeCharacter(final char character) {
        switch (character) {
        case '"':
        case '\'':
            return '\\';
        default:
            return 0;
        }
    }

    private class ExitPolicy implements IExitPolicy {

        final char fExitCharacter;
        final char fEscapeCharacter;
        final Stack<BracketLevel> fStack;
        final int fSize;

        public ExitPolicy(final char exitCharacter, final char escapeCharacter,
                final Stack<BracketLevel> stack) {
            fExitCharacter = exitCharacter;
            fEscapeCharacter = escapeCharacter;
            fStack = stack;
            fSize = fStack.size();
        }

        /*
         * @see
         * org.eclipse.jdt.internal.ui.text.link.LinkedPositionUI.ExitPolicy
         * #doExit(org.eclipse.jdt.internal.ui.text.link.LinkedPositionManager,
         * org.eclipse.swt.events.VerifyEvent, int, int)
         */
        @Override
        @SuppressWarnings("synthetic-access")
        public ExitFlags doExit(final LinkedModeModel model,
                final VerifyEvent event, final int offset, final int length) {

            if (fSize == fStack.size() && !isMasked(offset)) {
                if (event.character == fExitCharacter) {
                    final BracketLevel level = fStack.peek();
                    if (level.fFirstPosition.offset > offset
                            || level.fSecondPosition.offset < offset) {
                        return null;
                    }
                    if (level.fSecondPosition.offset == offset && length == 0) {
                        // don't enter the character if if its the closing
                        // peer
                        return new ExitFlags(ILinkedModeListener.UPDATE_CARET,
                                false);
                    }
                }
                // when entering an anonymous class between the parenthesis', we
                // don't want to jump after the closing parenthesis when return
                // is pressed
                if (event.character == SWT.CR && offset > 0) {
                    final IDocument document = fSourceViewer.getDocument();
                    try {
                        if (document.getChar(offset - 1) == '{') {
                            return new ExitFlags(ILinkedModeListener.EXIT_ALL,
                                    true);
                        }
                    } catch (final BadLocationException e) {
                    }
                }
            }
            return null;
        }

        @SuppressWarnings("synthetic-access")
        private boolean isMasked(final int offset) {
            final IDocument document = fSourceViewer.getDocument();
            try {
                return fEscapeCharacter == document.getChar(offset - 1);
            } catch (final BadLocationException e) {
            }
            return false;
        }
    }

    public ErlangEditorBracketInserter(final ErlangEditor erlangEditor,
            final ISourceViewer sourceViewer) {
        super();
        fErlangEditor = erlangEditor;
        fSourceViewer = sourceViewer;
    }
}
