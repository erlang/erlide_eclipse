/*******************************************************************************
 * Copyright (c) 2009 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.console;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.ITokenScanner;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.erlide.backend.console.IoRequest;
import org.erlide.ui.editors.erl.ColorManager;
import org.erlide.ui.util.IColorManager;

public class ConsoleOutputScanner implements ITokenScanner {

    protected final IColorManager fColorManager;
    private boolean gotten;
    private String text;
    private int offset;

    public ConsoleOutputScanner(final ColorManager colorManager) {
        fColorManager = colorManager;
    }

    @Override
    public int getTokenLength() {
        return text.length();
    }

    @Override
    public int getTokenOffset() {
        return offset;
    }

    @Override
    public IToken nextToken() {
        if (gotten) {
            return Token.EOF;
        } else {
            gotten = true;
            if (text == null) {
                return Token.EOF;
            }
            return new Token(getAttribute(text));
        }
    }

    private TextAttribute getAttribute(final String string) {
        int style = SWT.NORMAL;
        Color bg = null;
        Color fg = null;
        if (IoRequest.RE_HEADER.matcher(string).lookingAt()) {
            fg = fColorManager.getColor(new RGB(80, 80, 100));
            bg = fColorManager.getColor(new RGB(255, 255, 230));
            style = SWT.BOLD;
        } else if (IoRequest.RE_PROMPT.matcher(string).matches()) {
            fg = fColorManager.getColor(new RGB(80, 80, 80));
            style = SWT.BOLD;
        }
        return new TextAttribute(fg, bg, style);
    }

    @Override
    public void setRange(final IDocument document, final int offset,
            final int length) {
        this.offset = offset;
        if (document instanceof ErlConsoleDocument) {
            // ErlConsoleDocument doc = (ErlConsoleDocument) document;
            // BackendShell shell = doc.getShell();
            // shell.getRequests(offset, length);
        }
        try {
            text = document.get(offset, length);
        } catch (final BadLocationException e) {
            text = null;
        }

        gotten = false;
    }

}
