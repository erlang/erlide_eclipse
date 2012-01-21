/*
 * Code borrowed from PyDev
 */
/*
 * @author Fabio Zadrozny
 * Created: June 2005
 * License: Common Public License v1.0
 */

package org.erlide.ui.util;

import java.io.Reader;
import java.io.StringReader;
import java.util.List;

import javax.swing.text.Document;
import javax.swing.text.EditorKit;
import javax.swing.text.html.HTMLEditorKit;

import org.erlide.utils.StringUtils;

public class UIStringUtils {

    private UIStringUtils() {
    }

    public static final Object EMPTY = "";

    public static String removeWhitespaceColumnsToLeft(final String hoverInfo) {
        final StringBuilder buf = new StringBuilder();
        int firstCharPosition = Integer.MAX_VALUE;

        final List<String> splitted = StringUtils.splitLines(hoverInfo);
        for (final String line : splitted) {
            if (line.trim().length() > 0) {
                final int found = ErlideSelection.getFirstCharPosition(line);
                firstCharPosition = Math.min(found, firstCharPosition);
            }
        }

        if (firstCharPosition != Integer.MAX_VALUE) {
            for (final String line : splitted) {
                if (line.length() > firstCharPosition) {
                    buf.append(line.substring(firstCharPosition));
                }
            }
            return buf.toString();
        } else {
            return hoverInfo;// return initial
        }
    }

    /**
     * Given some html, extracts its text.
     */
    public static String extractTextFromHTML(final String html) {
        try {
            final EditorKit kit = new HTMLEditorKit();
            final Document doc = kit.createDefaultDocument();

            // The Document class does not yet handle charset's properly.
            doc.putProperty("IgnoreCharsetDirective", Boolean.TRUE);

            // Create a reader on the HTML content.
            final Reader rd = new StringReader(html);

            // Parse the HTML.
            kit.read(rd, doc, 0);

            // The HTML text is now stored in the document
            return doc.getText(0, doc.getLength());
        } catch (final Exception e) {
        }
        return "";
    }

}
