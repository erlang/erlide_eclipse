/*
 * Code borrowed from PyDev
 */
/*
 * @author Fabio Zadrozny
 * Created: June 2004
 * License: Common Public License v1.0
 */

package org.erlide.ui.information;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.Iterator;

import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.TextPresentation;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.graphics.Drawable;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.widgets.Display;
import org.erlide.core.util.StringUtils;
import org.erlide.ui.util.ErlLineBreakReader;
import org.erlide.ui.util.UIStringUtils;

/**
 * Based on HTMLTextPresenter
 * 
 * @author Fabio
 */
public class ErlInformationPresenter implements
        DefaultInformationControl.IInformationPresenter,
        DefaultInformationControl.IInformationPresenterExtension {

    public static final String LINE_DELIM = System.getProperty(
            "line.separator", "\n"); //$NON-NLS-1$ //$NON-NLS-2$

    private int fCounter;
    private final boolean fEnforceUpperLineLimit;

    public ErlInformationPresenter(boolean enforceUpperLineLimit) {
        super();
        fEnforceUpperLineLimit = enforceUpperLineLimit;
    }

    public ErlInformationPresenter() {
        this(true);
    }

    /**
     * Creates the reader and properly puts the presentation into place.
     */
    protected Reader createReader(String hoverInfo,
            TextPresentation presentation) {
        String str = UIStringUtils.removeWhitespaceColumnsToLeft(hoverInfo);

        str = correctLineDelimiters(str);
        str = makeEdocBold(presentation, str);

        return new StringReader(str);
    }

    /**
     * The line delimiters must match the platform for the bolds to be correct,
     * so, in this function we remove the ones existing and add the ones
     * dependent on the platform
     */
    private String correctLineDelimiters(String str) {
        StringBuffer buf = new StringBuffer();
        for (String s : StringUtils.splitLines(str)) {

            boolean found = false;
            while (s.endsWith("\r") || s.endsWith("\n")) {
                found = true;
                s = s.substring(0, s.length() - 1);
            }
            buf.append(s);
            if (found) {
                buf.append(LINE_DELIM);
            }
        }
        str = buf.toString();
        return str;
    }

    /**
     * Changes the @xxx bbb: things for bold
     */
    private String makeEdocBold(TextPresentation presentation, String str) {
        int lastIndex = 0;

        // 1st, let's mark in bold the things generated in edoc.
        while (true) {
            int start = str.indexOf('@', lastIndex);
            if (start == -1) {
                break;
            }
            int end = start + 1;
            while (end < str.length()) {
                if (!(str.charAt(end) == ':')) {
                    end++;
                } else {
                    break;
                }
            }
            if (end == start) {
                break;
            }
            lastIndex = end;
            presentation.addStyleRange(new StyleRange(start, end - start, null,
                    null, SWT.BOLD));
        }

        // return the input (this one doesn't change the string)
        return str;
    }

    @SuppressWarnings("unchecked")
    protected void adaptTextPresentation(TextPresentation presentation,
            int offset, int insertLength) {

        int yoursStart = offset;
        int yoursEnd = offset + insertLength - 1;
        yoursEnd = Math.max(yoursStart, yoursEnd);

        Iterator<StyleRange> e = presentation.getAllStyleRangeIterator();
        while (e.hasNext()) {

            StyleRange range = e.next();

            int myStart = range.start;
            int myEnd = range.start + range.length - 1;
            myEnd = Math.max(myStart, myEnd);

            if (myEnd < yoursStart) {
                continue;
            }

            if (myStart < yoursStart) {
                range.length += insertLength;
            } else {
                range.start += insertLength;
            }
        }
    }

    private void append(StringBuffer buffer, String string,
            TextPresentation presentation) {

        int length = string.length();
        buffer.append(string);

        if (presentation != null) {
            adaptTextPresentation(presentation, fCounter, length);
        }

        fCounter += length;
    }

    private String getIndent(String line) {
        int length = line.length();

        int i = 0;
        while (i < length && Character.isWhitespace(line.charAt(i))) {
            ++i;
        }

        return ((i == length) ? line : line.substring(0, i)) + " "; //$NON-NLS-1$
    }

    /*
     * @see IHoverInformationPresenter#updatePresentation(Display display,
     * String, TextPresentation, int, int)
     */
    public String updatePresentation(Display display, String hoverInfo,
            TextPresentation presentation, int maxWidth, int maxHeight) {
        return updatePresentation((Drawable) display, hoverInfo, presentation,
                maxWidth, maxHeight);
    }

    /*
     * @see IHoverInformationPresenterExtension#updatePresentation(Drawable
     * drawable, String, TextPresentation, int, int)
     * 
     * @since 3.2
     */
    public String updatePresentation(Drawable drawable, String hoverInfo,
            TextPresentation presentation, int maxWidth, int maxHeight) {

        if (hoverInfo == null) {
            return null;
        }

        GC gc = new GC(drawable);
        try {

            StringBuffer buffer = new StringBuffer();
            int maxNumberOfLines = Math.round((float) maxHeight
                    / (float) gc.getFontMetrics().getHeight());

            fCounter = 0;
            ErlLineBreakReader reader = new ErlLineBreakReader(createReader(
                    hoverInfo, presentation), gc, maxWidth);

            boolean lastLineFormatted = false;
            String lastLineIndent = null;

            String line = reader.readLine();
            boolean lineFormatted = reader.isFormattedLine();
            boolean firstLineProcessed = false;

            while (line != null) {

                if (fEnforceUpperLineLimit && maxNumberOfLines <= 0) {
                    break;
                }

                if (firstLineProcessed) {
                    if (!lastLineFormatted) {
                        append(buffer, LINE_DELIM, null);
                    } else {
                        append(buffer, LINE_DELIM, presentation);
                        if (lastLineIndent != null) {
                            append(buffer, lastLineIndent, presentation);
                        }
                    }
                }

                append(buffer, line, null);
                firstLineProcessed = true;

                lastLineFormatted = lineFormatted;
                if (!lineFormatted) {
                    lastLineIndent = null;
                } else if (lastLineIndent == null) {
                    lastLineIndent = getIndent(line);
                }

                line = reader.readLine();
                lineFormatted = reader.isFormattedLine();

                maxNumberOfLines--;
            }

            if (line != null) {
                append(buffer, LINE_DELIM, lineFormatted ? presentation : null);
            }
            return trim(buffer, presentation);

        } catch (IOException e) {
            // ignore TODO do something else?
            return null;
        } finally {
            gc.dispose();
        }
    }

    private String trim(StringBuffer buffer, TextPresentation presentation) {
        int length = buffer.length();

        int end = length - 1;
        while (end >= 0 && Character.isWhitespace(buffer.charAt(end))) {
            --end;
        }

        if (end == -1) {
            return ""; //$NON-NLS-1$
        }

        if (end < length - 1) {
            buffer.delete(end + 1, length);
        } else {
            end = length;
        }

        int start = 0;
        while (start < end && Character.isWhitespace(buffer.charAt(start))) {
            ++start;
        }

        buffer.delete(0, start);
        presentation.setResultWindow(new Region(start, buffer.length()));
        return buffer.toString();
    }
}
