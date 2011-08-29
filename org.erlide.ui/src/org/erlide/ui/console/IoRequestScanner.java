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

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.rules.IPartitionTokenScanner;
import org.eclipse.jface.text.rules.IToken;
import org.erlide.core.backend.console.IBackendShell;
import org.erlide.core.backend.console.IoRequest;

public class IoRequestScanner implements IPartitionTokenScanner {

    private final IBackendShell model;
    private int docOffset;
    private int docLength;
    private int crtOffset;
    private int crtLength;

    public IoRequestScanner(final IBackendShell model) {
        Assert.isNotNull(model);
        this.model = model;
    }

    public void setPartialRange(final IDocument document, final int offset,
            final int length, final String contentType,
            final int partitionOffset) {
        docOffset = offset;
        docLength = length;
        final IoRequest req = model.findAtPos(docOffset);
        if (req != null) {
            crtOffset = req.getStart();
        } else {
            crtOffset = -1;
        }
        crtLength = 0;
    }

    public int getTokenLength() {
        if (crtOffset + crtLength > docLength) {
            return crtLength - (docLength - crtOffset);
        }
        return crtLength;
    }

    public int getTokenOffset() {
        return crtOffset;
    }

    public IToken nextToken() {
        IoRequest req;
        crtOffset = crtOffset + crtLength;
        if (crtOffset > docOffset + docLength) {
            return new IoRequestToken(null);
        }
        req = model.findAtPos(crtOffset);
        if (req != null) {
            crtLength = req.getLength();
        }
        final IoRequestToken token = new IoRequestToken(req);
        return token;
    }

    public void setRange(final IDocument document, final int offset,
            final int length) {
        docOffset = offset;
        docLength = length;
        final IoRequest req = model.findAtPos(docOffset);
        if (req != null) {
            crtOffset = req.getStart();
        } else {
            crtOffset = -1;
        }
        crtLength = 0;
    }

    private static class IoRequestToken implements IToken {

        private final String data;

        public IoRequestToken(final IoRequest req) {
            if (req == null) {
                data = null;
            } else {
                data = req.getKind().name();
            }
        }

        public Object getData() {
            return data;
        }

        public boolean isEOF() {
            return data == null;
        }

        public boolean isOther() {
            return true;
        }

        public boolean isUndefined() {
            return false;
        }

        public boolean isWhitespace() {
            return false;
        }

        @Override
        public String toString() {
            return "TOK:" + data;
        }
    }
}
