/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.erl.scanner;

import java.util.List;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.rules.IToken;
import org.erlide.model.erlang.ErlToken;
import org.erlide.model.internal.erlang.ErlideScanner;
import org.erlide.model.internal.erlang.ScannerException;
import org.erlide.ui.editors.erl.ErlTokenScanner;
import org.erlide.ui.util.IColorManager;
import org.erlide.util.ErlLogger;

// TODO use RuleBasedScanner instead?
public class ErlCodeScanner extends ErlTokenScanner {

    protected List<ErlToken> fTokens;
    protected int fCrtToken;
    private int rangeLength;
    private int rangeOffset;

    public ErlCodeScanner(final IColorManager colorManager) {
        super(colorManager);
    }

    @Override
    public void setRange(final IDocument document, final int offset,
            final int length) {
        if (document == null) {
            return;
        }
        try {
            rangeOffset = offset;
            rangeLength = length;

            // ErlLogger.debug("setRange %s %d:%d (%d:%d)", document,
            // rangeOffset,
            // rangeLength, offset, length);

            final String str = document.get(rangeOffset, rangeLength);
            setText(str);
        } catch (final BadLocationException e) {
            ErlLogger.warn(e);
        }

    }

    private void setText(final String text) {
        if (text == null) {
            return;
        }

        try {
            fCrtToken = -1;

            final String str = text;
            fTokens = ErlideScanner.lightScanString(str, rangeOffset);

        } catch (final ScannerException e) {
            // e.printStackTrace();
        }
    }

    @Override
    public IToken nextToken() {
        final ErlToken nextErlToken = nextErlToken();
        return convert(nextErlToken);
    }

    @Override
    public int getTokenOffset() {
        if (fTokens == null) {
            return 0;
        }

        if (fCrtToken >= fTokens.size()) {
            return 0;
        }

        final ErlToken tk = fTokens.get(fCrtToken);
        return tk.getOffset();
    }

    @Override
    public int getTokenLength() {
        if (fTokens == null) {
            return 0;
        }

        if (fCrtToken >= fTokens.size()) {
            return 0;
        }

        final ErlToken tk = fTokens.get(fCrtToken);
        return tk.getLength();
    }

    public ErlToken nextErlToken() {
        if (fTokens == null) {
            return ErlToken.EOF;
        }

        fCrtToken++;
        if (fCrtToken >= fTokens.size()) {
            return ErlToken.EOF;
        }

        final ErlToken tk = fTokens.get(fCrtToken);
        if (tk.getOffset() >= rangeOffset + rangeLength) {
            return ErlToken.EOF;
        }
        return fTokens.get(fCrtToken);
    }
}
