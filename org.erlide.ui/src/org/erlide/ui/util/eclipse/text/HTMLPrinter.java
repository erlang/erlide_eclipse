/*******************************************************************************
 * Copyright (c) 2000, 2008 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.util.eclipse.text;

import java.io.IOException;
import java.io.Reader;
import java.net.URL;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTError;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.osgi.framework.Bundle;

import com.google.common.base.Charsets;

/**
 * Provides a set of convenience methods for creating HTML pages.
 * <p>
 * Moved into this package from
 * <code>org.eclipse.jface.internal.text.revisions</code>.
 * </p>
 */
public class HTMLPrinter {

    static RGB BG_COLOR_RGB = new RGB(255, 255, 225);
    static RGB FG_COLOR_RGB = new RGB(0, 0, 0);
    private static FontData fontData;
    private static URL fgStyleSheet;

    static {
        final Display display = Display.getDefault();
        if (display != null && !display.isDisposed()) {
            try {
                display.asyncExec(new Runnable() {
                    /*
                     * @see java.lang.Runnable#run()
                     */
                    @Override
                    public void run() {
                        BG_COLOR_RGB = display.getSystemColor(
                                SWT.COLOR_INFO_BACKGROUND).getRGB();
                        FG_COLOR_RGB = display.getSystemColor(
                                SWT.COLOR_INFO_FOREGROUND).getRGB();
                    }
                });
            } catch (final SWTError err) {
                // see: https://bugs.eclipse.org/bugs/show_bug.cgi?id=45294
                if (err.code != SWT.ERROR_DEVICE_DISPOSED) {
                    throw err;
                }
            }
        }
        initStyleSheet();
    }

    private HTMLPrinter() {
    }

    private static String replace(final String text, final char c,
            final String s) {

        int previous = 0;
        int current = text.indexOf(c, previous);

        if (current == -1) {
            return text;
        }

        final StringBuffer buffer = new StringBuffer();
        while (current > -1) {
            buffer.append(text.substring(previous, current));
            buffer.append(s);
            previous = current + 1;
            current = text.indexOf(c, previous);
        }
        buffer.append(text.substring(previous));

        return buffer.toString();
    }

    public static String convertToHTMLContent(String content) {
        content = replace(content, '&', "&amp;"); //$NON-NLS-1$
        content = replace(content, '"', "&quot;"); //$NON-NLS-1$
        content = replace(content, '<', "&lt;"); //$NON-NLS-1$
        return replace(content, '>', "&gt;"); //$NON-NLS-1$
    }

    public static String read(final Reader rd) {

        final StringBuffer buffer = new StringBuffer();
        final char[] readBuffer = new char[2048];

        try {
            int n = rd.read(readBuffer);
            while (n > 0) {
                buffer.append(readBuffer, 0, n);
                n = rd.read(readBuffer);
            }
            return buffer.toString();
        } catch (final IOException x) {
        }

        return null;
    }

    private static void appendColors(final StringBuffer pageProlog,
            final RGB fgRGB, final RGB bgRGB) {
        pageProlog.append("<body text=\""); //$NON-NLS-1$
        appendColor(pageProlog, fgRGB);
        pageProlog.append("\" bgcolor=\""); //$NON-NLS-1$
        appendColor(pageProlog, bgRGB);
        pageProlog.append("\">"); //$NON-NLS-1$
    }

    private static void appendColor(final StringBuffer buffer, final RGB rgb) {
        buffer.append('#');
        appendAsHexString(buffer, rgb.red);
        appendAsHexString(buffer, rgb.green);
        appendAsHexString(buffer, rgb.blue);
    }

    private static void appendAsHexString(final StringBuffer buffer,
            final int intValue) {
        final String hexValue = Integer.toHexString(intValue);
        if (hexValue.length() == 1) {
            buffer.append('0');
        }
        buffer.append(hexValue);
    }

    public static void insertStyles(final StringBuffer buffer,
            final String[] styles) {
        if (styles == null || styles.length == 0) {
            return;
        }

        final StringBuffer styleBuf = new StringBuffer(10 * styles.length);
        for (int i = 0; i < styles.length; i++) {
            styleBuf.append(" style=\""); //$NON-NLS-1$
            styleBuf.append(styles[i]);
            styleBuf.append('"');
        }

        // Find insertion index
        // a) within existing body tag with trailing space
        int index = buffer.indexOf("<body "); //$NON-NLS-1$
        if (index != -1) {
            buffer.insert(index + 5, styleBuf);
            return;
        }

        // b) within existing body tag without attributes
        index = buffer.indexOf("<body>"); //$NON-NLS-1$
        if (index != -1) {
            buffer.insert(index + 5, ' ');
            buffer.insert(index + 6, styleBuf);
            return;
        }
    }

    private static void appendStyleSheetURL(final StringBuffer buffer,
            final URL styleSheetURL) {
        if (styleSheetURL == null) {
            return;
        }

        buffer.append("<head>"); //$NON-NLS-1$
        buffer.append("<meta charset='utf-8'>");

        buffer.append("<link rel=\"stylesheet\" href= \""); //$NON-NLS-1$
        buffer.append(styleSheetURL);
        buffer.append("\" charset=\"ISO-8859-1\" type=\"text/css\">"); //$NON-NLS-1$

        buffer.append("</head>"); //$NON-NLS-1$
    }

    public static void insertPageProlog(final StringBuffer buffer,
            final int position, final URL styleSheetURL) {
        final StringBuffer pageProlog = new StringBuffer(300);
        updateDialogFontData();
        pageProlog.append("<html>"); //$NON-NLS-1$
        if (styleSheetURL != null) {
            appendStyleSheetURL(pageProlog, styleSheetURL);
        }
        appendFontData(pageProlog);
        appendColors(pageProlog, FG_COLOR_RGB, BG_COLOR_RGB);
        buffer.insert(position, pageProlog.toString());
    }

    private static void appendFontData(final StringBuffer buffer) {
        final String size = Integer.toString(fontData.getHeight())
                + ("carbon".equals(SWT.getPlatform()) ? "px" : "pt");
        buffer.append("<style type=\"text/css\">body {font-size:").append(size)
                .append("; font-family:'").append(fontData.getName())
                .append("',serif;}</style>");
    }

    public static void updateDialogFontData() {
        final Runnable runnable = new Runnable() {
            @Override
            public void run() {
                fontData = JFaceResources.getTextFont().getFontData()[0];
            }
        };
        final Display display = Display.getDefault();
        // have to execute in UI thread only
        if (display.getThread() != Thread.currentThread()) {
            display.syncExec(runnable);
        } else {
            runnable.run();
        }
    }

    public static void addPageEpilog(final StringBuffer buffer) {
        buffer.append("</body></html>"); //$NON-NLS-1$
    }

    public static void startBulletList(final StringBuffer buffer) {
        buffer.append("<ul>"); //$NON-NLS-1$
    }

    public static void endBulletList(final StringBuffer buffer) {
        buffer.append("</ul>"); //$NON-NLS-1$
    }

    public static void addBullet(final StringBuffer buffer, final String bullet) {
        if (bullet != null) {
            buffer.append("<li>"); //$NON-NLS-1$
            buffer.append(bullet);
            buffer.append("</li>"); //$NON-NLS-1$
        }
    }

    public static void addSmallHeader(final StringBuffer buffer,
            final String header) {
        if (header != null) {
            buffer.append("<h5>"); //$NON-NLS-1$
            buffer.append(header);
            buffer.append("</h5>"); //$NON-NLS-1$
        }
    }

    public static void addParagraph(final StringBuffer buffer,
            final String paragraph) {
        if (paragraph != null) {
            buffer.append("<p>"); //$NON-NLS-1$
            buffer.append(paragraph);
        }
    }

    public static void addParagraph(final StringBuffer buffer,
            final Reader paragraphReader) {
        if (paragraphReader != null) {
            addParagraph(buffer, read(paragraphReader));
        }
    }

    public static String fixEncoding(final String comment) {
        try {
            final byte[] bytes = comment.getBytes(Charsets.ISO_8859_1);
            final ByteBuffer bb = ByteBuffer.wrap(bytes);
            final CharBuffer cb = Charsets.UTF_8.newDecoder().decode(bb);
            return cb.toString();
        } catch (final Exception e) {
            // it was Latin-1
        }
        return comment;
    }

    public static String asHtml(final String string) {
        final StringBuffer sb = new StringBuffer(string);
        if (sb.length() > 0) {
            insertPageProlog(sb, 0, fgStyleSheet);
            addPageEpilog(sb);
        }
        final String result = sb.toString().replace("\u00C2\u00A0", "&nbsp;");
        return result;
    }

    private static void initStyleSheet() {
        final Bundle bundle = Platform.getBundle(ErlideUIPlugin.PLUGIN_ID);
        fgStyleSheet = bundle.getEntry("/edoc.css"); //$NON-NLS-1$
        if (fgStyleSheet != null) {
            try {
                fgStyleSheet = FileLocator.toFileURL(fgStyleSheet);
            } catch (final Exception e) {
            }
        }
    }

}
