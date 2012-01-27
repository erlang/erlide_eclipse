/*
 * Code borrowed from PyDev
 *
 * @author: ptoofani
 * @author Fabio Zadrozny
 * Created: June 2004
 * License: Common Public License v1.0
 */

package org.erlide.ui.util;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Pattern;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.text.TextUtilities;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.jinterface.ErlLogger;
import org.erlide.utils.Tuple;

/**
 * Used as 'shortcuts' to document and selection settings.
 * 
 * @author Fabio Zadrozny
 * @author Parhaum Toofanian
 */
public class ErlideSelection {

    private final IDocument doc;
    private ITextSelection textSelection;

    /**
     * Alternate constructor for ErlideSelection. Takes in a text editor from
     * Eclipse.
     * 
     * @param textEditor
     *            The text editor operating in Eclipse
     */
    public ErlideSelection(final ITextEditor textEditor) {
        this(textEditor.getDocumentProvider().getDocument(
                textEditor.getEditorInput()), (ITextSelection) textEditor
                .getSelectionProvider().getSelection());
    }

    /**
     * @param document
     *            the document we are using to make the selection
     * @param selection
     *            that's the actual selection. It might have an offset and a
     *            number of selected chars
     */
    public ErlideSelection(final IDocument doc, final ITextSelection selection) {
        this.doc = doc;
        textSelection = selection;
    }

    /**
     * Creates a selection from a document
     * 
     * @param doc
     *            the document to be used
     * @param line
     *            the line (starts at 0)
     * @param col
     *            the col (starts at 0)
     */
    public ErlideSelection(final IDocument doc, final int line, final int col) {
        this(doc, line, col, 0);
    }

    public ErlideSelection(final IDocument doc, final int line, final int col,
            final int len) {
        this.doc = doc;
        textSelection = new TextSelection(doc, getAbsoluteCursorOffset(line,
                col), len);
    }

    public static int getAbsoluteCursorOffset(final IDocument doc,
            final int line, final int col) {
        try {
            final IRegion offsetR = doc.getLineInformation(line);
            return offsetR.getOffset() + col;
        } catch (final Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * @param line
     *            0-based
     * @param col
     *            0-based
     * @return the absolute cursor offset in the contained document
     */
    public int getAbsoluteCursorOffset(final int line, final int col) {
        return getAbsoluteCursorOffset(doc, line, col);
    }

    /**
     * @param document
     *            the document we are using to make the selection
     * @param offset
     *            the offset where the selection will happen (0 characters will
     *            be selected)
     */
    public ErlideSelection(final IDocument doc, final int offset) {
        this.doc = doc;
        textSelection = new TextSelection(doc, offset, 0);
    }

    /**
     * Changes the selection
     * 
     * @param absoluteStart
     *            this is the offset of the start of the selection
     * @param absoluteEnd
     *            this is the offset of the end of the selection
     */
    public void setSelection(final int absoluteStart, final int absoluteEnd) {
        textSelection = new TextSelection(doc, absoluteStart, absoluteEnd
                - absoluteStart);
    }

    /**
     * Creates a selection for the document, so that no characters are selected
     * and the offset is position 0
     * 
     * @param doc
     *            the document where we are doing the selection
     */
    public ErlideSelection(final IDocument doc) {
        this(doc, 0);
    }

    /**
     * In event of partial selection, used to select the full lines involved.
     */
    public void selectCompleteLine() {
        final IRegion endLine = getEndLine();
        final IRegion startLine = getStartLine();

        textSelection = new TextSelection(doc, startLine.getOffset(),
                endLine.getOffset() + endLine.getLength()
                        - startLine.getOffset());
    }

    protected static void beep(final Exception e) {
        ErlLogger.info(e);
        PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell()
                .getDisplay().beep();
    }

    /**
     * @return the current column that is selected from the cursor.
     */
    public int getCursorColumn() {
        try {
            final int absoluteOffset = getAbsoluteCursorOffset();
            final IRegion region = doc
                    .getLineInformationOfOffset(absoluteOffset);
            return absoluteOffset - region.getOffset();
        } catch (final BadLocationException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Gets current line from document.
     * 
     * @return String line in String form
     */
    public String getLine() {
        return getLine(getDoc(), getCursorLine());
    }

    /**
     * Gets line from document.
     * 
     * @param i
     *            Line number
     * @return String line in String form
     */
    public String getLine(final int i) {
        return getLine(getDoc(), i);
    }

    /**
     * Gets line from document.
     * 
     * @param i
     *            Line number
     * @return String line in String form
     */
    public static String getLine(final IDocument doc, final int i) {
        try {
            return doc.get(doc.getLineInformation(i).getOffset(), doc
                    .getLineInformation(i).getLength());
        } catch (final Exception e) {
            return "";
        }
    }

    public int getLineOfOffset() {
        return getLineOfOffset(this.getAbsoluteCursorOffset());
    }

    public int getLineOfOffset(final int offset) {
        return getLineOfOffset(getDoc(), offset);
    }

    /**
     * @param offset
     *            the offset we want to get the line
     * @return the line of the passed offset
     */
    public static int getLineOfOffset(final IDocument doc, final int offset) {
        try {
            return doc.getLineOfOffset(offset);
        } catch (final BadLocationException e) {
            return 0;
        }
    }

    /**
     * @return the offset of the line where the cursor is
     */
    public int getLineOffset() {
        return getLineOffset(getCursorLine());
    }

    /**
     * @return the offset of the specified line
     */
    public int getLineOffset(final int line) {
        try {
            return getDoc().getLineInformation(line).getOffset();
        } catch (final Exception e) {
            return 0;
        }
    }

    /**
     * Deletes a line from the document
     * 
     * @param i
     */
    public void deleteLine(final int i) {
        deleteLine(getDoc(), i);
    }

    /**
     * Deletes a line from the document
     * 
     * @param i
     */
    public static void deleteLine(final IDocument doc, final int i) {
        try {
            final IRegion lineInformation = doc.getLineInformation(i);
            final int offset = lineInformation.getOffset();

            int length = -1;

            if (doc.getNumberOfLines() > i) {
                final int nextLineOffset = doc.getLineInformation(i + 1)
                        .getOffset();
                length = nextLineOffset - offset;
            } else {
                length = lineInformation.getLength();
            }

            if (length > -1) {
                doc.replace(offset, length, "");
            }
        } catch (final BadLocationException e) {
            e.printStackTrace();
        }
    }

    public void deleteSpacesAfter(final int offset) {
        try {
            final int len = countSpacesAfter(offset);
            if (len > 0) {
                doc.replace(offset, len, "");
            }
        } catch (final Exception e) {
            // ignore
        }
    }

    public int countSpacesAfter(int offset) throws BadLocationException {
        if (offset >= doc.getLength()) {
            return 0;
        }

        final int initial = offset;
        String next = doc.get(offset, 1);

        // don't delete 'all' that is considered whitespace (as \n and \r)
        try {
            while (next.charAt(0) == ' ' || next.charAt(0) == '\t') {
                offset++;
                next = doc.get(offset, 1);
            }
        } catch (final Exception e) {
            // ignore
        }

        return offset - initial;
    }

    /**
     * Deletes the current selected text
     * 
     * @throws BadLocationException
     */
    public void deleteSelection() throws BadLocationException {
        final int offset = textSelection.getOffset();
        doc.replace(offset, textSelection.getLength(), "");
    }

    public void addLine(final String contents, final int afterLine) {
        addLine(getDoc(), getEndLineDelim(), contents, afterLine);
    }

    /**
     * Adds a line to the document.
     * 
     * @param doc
     *            the document
     * @param endLineDelim
     *            the delimiter that should be used
     * @param contents
     *            what should be added (the end line delimiter may be added
     *            before or after those contents (depending on what are the
     *            current contents of the document).
     * @param afterLine
     *            the contents should be added after the line specified here.
     */
    public static void addLine(final IDocument doc, final String endLineDelim,
            String contents, final int afterLine) {
        try {

            int offset = -1;
            if (doc.getNumberOfLines() > afterLine) {
                offset = doc.getLineInformation(afterLine + 1).getOffset();

            } else {
                offset = doc.getLineInformation(afterLine).getOffset();
            }

            if (doc.getNumberOfLines() - 1 == afterLine) {
                contents = endLineDelim + contents;

            }

            if (!contents.endsWith(endLineDelim)) {
                contents += endLineDelim;
            }

            if (offset >= 0) {
                doc.replace(offset, 0, contents);
            }
        } catch (final BadLocationException e) {
            e.printStackTrace();
        }
    }

    /**
     * @return the line where the cursor is (from the cursor position to the end
     *         of the line).
     * @throws BadLocationException
     */
    public String getLineContentsFromCursor() throws BadLocationException {
        final int lineOfOffset = getDoc().getLineOfOffset(
                getAbsoluteCursorOffset());
        final IRegion lineInformation = getDoc().getLineInformation(
                lineOfOffset);

        final String lineToCursor = getDoc().get(
                getAbsoluteCursorOffset(),
                lineInformation.getOffset() + lineInformation.getLength()
                        - getAbsoluteCursorOffset());
        return lineToCursor;
    }

    /**
     * @param ps
     * @return the line where the cursor is (from the beggining of the line to
     *         the cursor position).
     * @throws BadLocationException
     */
    public String getLineContentsToCursor() throws BadLocationException {
        final int lineOfOffset = getDoc().getLineOfOffset(
                getAbsoluteCursorOffset());
        final IRegion lineInformation = getDoc().getLineInformation(
                lineOfOffset);
        final String lineToCursor = getDoc().get(lineInformation.getOffset(),
                getAbsoluteCursorOffset() - lineInformation.getOffset());
        return lineToCursor;
    }

    /**
     * Readjust the selection so that the whole document is selected.
     * 
     * @param onlyIfNothingSelected
     *            : If false, check if we already have a selection. If we have a
     *            selection, it is not changed, however, if it is true, it
     *            always selects everything.
     */
    public void selectAll(final boolean forceNewSelection) {
        if (!forceNewSelection) {
            if (getSelLength() > 0) {
                return;
            }
        }

        textSelection = new TextSelection(doc, 0, doc.getLength());
    }

    /**
     * @return Returns the startLineIndex.
     */
    public int getStartLineIndex() {
        return getTextSelection().getStartLine();
    }

    /**
     * @return Returns the endLineIndex.
     */
    public int getEndLineIndex() {
        return getTextSelection().getEndLine();
    }

    /**
     * @return Returns the doc.
     */
    public IDocument getDoc() {
        return doc;
    }

    /**
     * @return Returns the selLength.
     */
    public int getSelLength() {
        return getTextSelection().getLength();
    }

    /**
     * @return Returns the selection.
     */
    public String getCursorLineContents() {
        try {
            final int start = getStartLine().getOffset();
            final int end = getEndLine().getOffset() + getEndLine().getLength();
            return doc.get(start, end - start);
        } catch (final BadLocationException e) {
            ErlLogger.info(e);
        }
        return "";
    }

    /**
     * @return the delimiter that should be used for the passed document
     */
    public static String getDelimiter(final IDocument doc) {
        return TextUtilities.getDefaultLineDelimiter(doc);
    }

    /**
     * @return Returns the endLineDelim.
     */
    public String getEndLineDelim() {
        return getDelimiter(getDoc());
    }

    /**
     * @return Returns the startLine.
     */
    public IRegion getStartLine() {
        try {
            return getDoc().getLineInformation(getStartLineIndex());
        } catch (final BadLocationException e) {
            ErlLogger.info(e);
        }
        return null;
    }

    /**
     * @return Returns the endLine.
     */
    public IRegion getEndLine() {
        try {
            final int endLineIndex = getEndLineIndex();
            if (endLineIndex == -1) {
                return null;
            }
            return getDoc().getLineInformation(endLineIndex);
        } catch (final BadLocationException e) {
            ErlLogger.info(e);
        }
        return null;
    }

    /**
     * @return Returns the cursorLine.
     */
    public int getCursorLine() {
        return getTextSelection().getEndLine();
    }

    /**
     * @return Returns the absoluteCursorOffset.
     */
    public int getAbsoluteCursorOffset() {
        return getTextSelection().getOffset();
    }

    /**
     * @return Returns the textSelection.
     */
    public ITextSelection getTextSelection() {
        return textSelection;
    }

    /**
     * @return the Selected text
     */
    public String getSelectedText() {
        final ITextSelection txtSel = getTextSelection();
        final int start = txtSel.getOffset();
        final int len = txtSel.getLength();
        try {
            return doc.get(start, len);
        } catch (final BadLocationException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * @return
     * @throws BadLocationException
     */
    public char getCharAfterCurrentOffset() throws BadLocationException {
        return getDoc().getChar(getAbsoluteCursorOffset() + 1);
    }

    /**
     * @return
     * @throws BadLocationException
     */
    public char getCharAtCurrentOffset() throws BadLocationException {
        return getDoc().getChar(getAbsoluteCursorOffset());
    }

    /**
     * @return the offset mapping to the end of the line passed as parameter.
     * @throws BadLocationException
     */
    public int getEndLineOffset(final int line) throws BadLocationException {
        final IRegion lineInformation = doc.getLineInformation(line);
        return lineInformation.getOffset() + lineInformation.getLength();
    }

    /**
     * @return the offset mapping to the end of the current 'end' line.
     */
    public int getEndLineOffset() {
        final IRegion endLine = getEndLine();
        return endLine.getOffset() + endLine.getLength();
    }

    /**
     * @return the offset mapping to the start of the current line.
     */
    public int getStartLineOffset() {
        final IRegion startLine = getStartLine();
        return startLine.getOffset();
    }

    /**
     * @return the current token and its initial offset for this token
     * @throws BadLocationException
     */
    @SuppressWarnings("boxing")
    public Tuple<String, Integer> getCurrToken() throws BadLocationException {
        final Tuple<String, Integer> tup = extractActivationToken(doc,
                getAbsoluteCursorOffset(), false);
        final String prefix = tup.first;

        // ok, now, get the rest of the token, as we already have its prefix

        final int start = tup.second - prefix.length();
        int end = start;
        while (doc.getLength() - 1 >= end) {
            final char ch = doc.getChar(end);
            if (Character.isJavaIdentifierPart(ch)) {
                end++;
            } else {
                break;
            }
        }
        final String post = doc.get(tup.second, end - tup.second);
        return new Tuple<String, Integer>(prefix + post, start);
    }

    /**
     * This function replaces all the contents in the current line before the
     * cursor for the contents passed as parameter
     */
    public void replaceLineContentsToSelection(final String newContents)
            throws BadLocationException {
        final int lineOfOffset = getDoc().getLineOfOffset(
                getAbsoluteCursorOffset());
        final IRegion lineInformation = getDoc().getLineInformation(
                lineOfOffset);
        getDoc().replace(lineInformation.getOffset(),
                getAbsoluteCursorOffset() - lineInformation.getOffset(),
                newContents);

    }

    /**
     * This function goes backward in the document searching for an 'if' and
     * returns the line that has it.
     * 
     * May return null if it was not found.
     */
    public String getPreviousLineThatStartsWithToken(final String[] tokens) {
        final DocIterator iterator = new DocIterator(false, this);
        while (iterator.hasNext()) {
            final String line = iterator.next();
            final String trimmed = line.trim();
            for (final String prefix : tokens) {
                if (trimmed.startsWith(prefix)) {
                    return line;
                }
            }
        }
        return null;
    }

    /**
     * @param theDoc
     * @param documentOffset
     * @return
     * @throws BadLocationException
     */
    public static int eatFuncCall(final IDocument theDoc, int documentOffset)
            throws BadLocationException {
        final String c = theDoc.get(documentOffset, 1);
        if (!c.equals(")")) {
            throw new AssertionError("Expecting ) to eat callable. Received: "
                    + c);
        }

        while (documentOffset > 0 && !theDoc.get(documentOffset, 1).equals("(")) {
            documentOffset -= 1;
        }

        return documentOffset;
    }

    /**
     * Checks if the activationToken ends with some char from cs.
     */
    public static boolean endsWithSomeChar(final char cs[],
            final String activationToken) {
        for (int i = 0; i < cs.length; i++) {
            if (activationToken.endsWith(cs[i] + "")) {
                return true;
            }
        }
        return false;

    }

    public static List<Integer> getLineStartOffsets(
            final String replacementString) {
        final ArrayList<Integer> ret = new ArrayList<Integer>();
        ret.add(0);// there is always a starting one at 0

        // we may have line breaks with \r\n, or only \n or \r
        for (int i = 0; i < replacementString.length(); i++) {
            char c = replacementString.charAt(i);
            if (c == '\r') {
                i++;
                int foundAt = i;

                if (i < replacementString.length()) {
                    c = replacementString.charAt(i);
                    if (c == '\n') {
                        // i++;
                        foundAt = i + 1;
                    }
                }
                ret.add(foundAt);

            } else if (c == '\n') {
                ret.add(i + 1);
            }
        }

        return ret;
    }

    public static List<Integer> getLineBreakOffsets(
            final String replacementString) {
        final ArrayList<Integer> ret = new ArrayList<Integer>();

        int ignoreNextNAt = -1;

        // we may have line breaks with \r\n, or only \n or \r
        for (int i = 0; i < replacementString.length(); i++) {
            final char c = replacementString.charAt(i);
            if (c == '\r') {
                ret.add(i);
                ignoreNextNAt = i + 1;

            } else if (c == '\n') {
                if (ignoreNextNAt != i) {
                    ret.add(i);
                }
            }
        }

        return ret;
    }

    /**
     * @return the number of line breaks in the passed string.
     */
    public static int countLineBreaks(final String replacementString) {
        int lineBreaks = 0;
        int ignoreNextNAt = -1;

        // we may have line breaks with \r\n, or only \n or \r
        for (int i = 0; i < replacementString.length(); i++) {
            final char c = replacementString.charAt(i);
            if (c == '\r') {
                lineBreaks++;
                ignoreNextNAt = i + 1;

            } else if (c == '\n') {
                if (ignoreNextNAt != i) {
                    lineBreaks++;
                }
            }
        }
        return lineBreaks;
    }

    /**
     * This function gets the activation token from the document given the
     * current cursor position.
     * 
     * @param document
     *            this is the document we want info on
     * @param offset
     *            this is the cursor position
     * @param getFullQualifier
     *            if true we get the full qualifier (even if it passes the
     *            current cursor location)
     * @return a tuple with the activation token and the cursor offset (may
     *         change if we need to get the full qualifier, otherwise, it is the
     *         same offset passed as a parameter).
     */
    public static Tuple<String, Integer> extractActivationToken(
            final IDocument document, int offset, final boolean getFullQualifier) {
        try {
            if (getFullQualifier) {
                // if we have to get the full qualifier, we'll have to walk the
                // offset (cursor) forward
                while (offset < document.getLength()) {
                    final char ch = document.getChar(offset);
                    if (Character.isJavaIdentifierPart(ch)) {
                        offset++;
                    } else {
                        break;
                    }

                }
            }
            int i = offset;

            if (i > document.getLength()) {
                return new Tuple<String, Integer>("", document.getLength()); //$NON-NLS-1$
            }

            while (i > 0) {
                final char ch = document.getChar(i - 1);
                if (!Character.isJavaIdentifierPart(ch)) {
                    break;
                }
                i--;
            }

            return new Tuple<String, Integer>(document.get(i, offset - i),
                    offset);
        } catch (final BadLocationException e) {
            return new Tuple<String, Integer>("", offset); //$NON-NLS-1$
        }
    }

    /**
     * @param c
     * @param string
     */
    public static boolean containsOnly(final char c, final String string) {
        for (int i = 0; i < string.length(); i++) {
            if (string.charAt(i) != c) {
                return false;
            }
        }
        return true;
    }

    /**
     * @param string
     *            the string we care about
     * @return true if the string passed is only composed of whitespaces (or
     *         characters that are regarded as whitespaces by
     *         Character.isWhitespace)
     */
    public static boolean containsOnlyWhitespaces(final String string) {
        for (int i = 0; i < string.length(); i++) {
            if (!Character.isWhitespace(string.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    /**
     * @param selection
     *            the text from where we want to get the indentation
     * @return a string representing the whitespaces and tabs befor the first
     *         char in the passed line.
     */
    public static String getIndentationFromLine(final String selection) {
        final int firstCharPosition = getFirstCharPosition(selection);
        return selection.substring(0, firstCharPosition);
    }

    public String getIndentationFromLine() {
        return getIndentationFromLine(getCursorLineContents());
    }

    /**
     * @param src
     * @return
     */
    public static int getFirstCharPosition(final String src) {
        int i = 0;
        boolean breaked = false;
        while (i < src.length()) {
            if (!Character.isWhitespace(src.charAt(i)) && src.charAt(i) != '\t') {
                i++;
                breaked = true;
                break;
            }
            i++;
        }
        if (!breaked) {
            i++;
        }
        return i - 1;
    }

    /**
     * @param doc
     * @param region
     * @return
     * @throws BadLocationException
     */
    public static int getFirstCharRelativePosition(final IDocument doc,
            final IRegion region) throws BadLocationException {
        final int offset = region.getOffset();
        final String src = doc.get(offset, region.getLength());

        return getFirstCharPosition(src);
    }

    /**
     * @param doc
     * @param cursorOffset
     * @return
     * @throws BadLocationException
     */
    public static int getFirstCharRelativeLinePosition(final IDocument doc,
            final int line) throws BadLocationException {
        IRegion region;
        region = doc.getLineInformation(line);
        return getFirstCharRelativePosition(doc, region);
    }

    /**
     * @param doc
     * @param cursorOffset
     * @return
     * @throws BadLocationException
     */
    public static int getFirstCharRelativePosition(final IDocument doc,
            final int cursorOffset) throws BadLocationException {
        IRegion region;
        region = doc.getLineInformationOfOffset(cursorOffset);
        return getFirstCharRelativePosition(doc, region);
    }

    /**
     * Returns the position of the first non whitespace char in the current
     * line.
     * 
     * @param doc
     * @param cursorOffset
     * @return position of the first character of the line (returned as an
     *         absolute offset)
     * @throws BadLocationException
     */
    public static int getFirstCharPosition(final IDocument doc,
            final int cursorOffset) throws BadLocationException {
        IRegion region;
        region = doc.getLineInformationOfOffset(cursorOffset);
        final int offset = region.getOffset();
        return offset + getFirstCharRelativePosition(doc, cursorOffset);
    }

    /**
     * Class to help iterating through the document
     */
    public static class DocIterator implements Iterator<String> {
        private int startingLine;
        private final boolean forward;
        private boolean isFirst = true;
        private final int numberOfLines;
        private int lastReturnedLine = -1;
        private final ErlideSelection ps;

        public DocIterator(final boolean forward, final ErlideSelection ps) {
            this(forward, ps, ps.getCursorLine(), true);
        }

        public DocIterator(final boolean forward, final ErlideSelection ps,
                final int startingLine, final boolean considerFirst) {
            this.startingLine = startingLine;
            this.forward = forward;
            numberOfLines = ps.getDoc().getNumberOfLines();
            this.ps = ps;
            if (!considerFirst) {
                isFirst = false;
            }
        }

        public int getCurrentLine() {
            return startingLine;
        }

        @Override
        public boolean hasNext() {
            if (forward) {
                return startingLine < numberOfLines;
            } else {
                return startingLine >= 0;
            }
        }

        /**
         * Note that the first thing it returns is the lineContents to cursor
         * (and only after that does it return from the full line -- if it is
         * iterating backwards).
         */
        @Override
        public String next() {
            try {
                String line;
                if (forward) {
                    line = ps.getLine(startingLine);
                    lastReturnedLine = startingLine;
                    startingLine++;
                } else {
                    if (isFirst) {
                        line = ps.getLineContentsToCursor();
                        isFirst = false;
                    } else {
                        line = ps.getLine(startingLine);
                    }
                    lastReturnedLine = startingLine;
                    startingLine--;
                }
                return line;
            } catch (final Exception e) {
                throw new RuntimeException(e);
            }
        }

        @Override
        public void remove() {
            throw new RuntimeException("Remove not implemented.");
        }

        public int getLastReturnedLine() {
            return lastReturnedLine;
        }
    }

    /**
     * @return if the offset is inside the region
     */
    public static boolean isInside(final int offset, final IRegion region) {
        if (offset >= region.getOffset()
                && offset <= region.getOffset() + region.getLength()) {
            return true;
        }
        return false;
    }

    /**
     * @return if the col is inside the initial col/len
     */
    public static boolean isInside(final int col, final int initialCol,
            final int len) {
        if (col >= initialCol && col <= initialCol + len) {
            return true;
        }
        return false;
    }

    /**
     * @return if the region passed is composed of a single line
     */
    public static boolean endsInSameLine(final IDocument document,
            final IRegion region) {
        try {
            final int startLine = document.getLineOfOffset(region.getOffset());
            final int end = region.getOffset() + region.getLength();
            final int endLine = document.getLineOfOffset(end);
            return startLine == endLine;
        } catch (final BadLocationException e) {
            return false;
        }
    }

    /**
     * @param offset
     *            the offset we want info on
     * @return a tuple with the line, col of the passed offset in the document
     */
    public Tuple<Integer, Integer> getLineAndCol(final int offset) {
        try {
            final IRegion region = doc.getLineInformationOfOffset(offset);
            final int line = doc.getLineOfOffset(offset);
            final int col = offset - region.getOffset();
            return new Tuple<Integer, Integer>(line, col);
        } catch (final BadLocationException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * @return the contents from the document starting at the cursor line until
     *         a colon is reached.
     */
    public String getToColon() {
        final StringBuffer buffer = new StringBuffer();

        for (int i = getLineOffset(); i < doc.getLength(); i++) {
            try {
                final char c = doc.getChar(i);
                buffer.append(c);
                if (c == ':') {
                    return buffer.toString();
                }
            } catch (final BadLocationException e) {
                throw new RuntimeException(e);
            }
        }
        return ""; // unable to find a colon
    }

    public static boolean isIdentifier(final String str) {
        return IdentifierPattern.matcher(str).matches();
    }

    private static final Pattern IdentifierPattern = Pattern.compile("\\w*");

    public static boolean isCommentLine(final String line) {
        for (int j = 0; j < line.length(); j++) {
            final char c = line.charAt(j);
            if (c != ' ') {
                if (c == '%') {
                    // ok, it starts with % (so, it is a comment)
                    return true;
                }
            }
        }
        return false;
    }

    public IRegion getRegion() {
        return new Region(textSelection.getOffset(), textSelection.getLength());
    }

}
