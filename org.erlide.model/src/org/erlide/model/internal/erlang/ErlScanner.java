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
package org.erlide.model.internal.erlang;

import org.erlide.model.erlang.ErlToken;
import org.erlide.model.erlang.IErlScanner;

/**
 * Erlang syntax scanner
 */
public class ErlScanner implements IErlScanner {
    private final String name;

    public ErlScanner(final String name) {
        this.name = name;
        ErlideScanner.create(name);
    }

    @Override
    public void initialScan(final String initialText, final String path,
            final boolean logging) {
        final String pathNotNull = path == null ? "" : path;
        ErlideScanner.initialScan(name, pathNotNull, initialText, logging);
    }

    @Override
    public void dispose() {
        ErlideScanner.dispose(name);
    }

    @Override
    public void replaceText(final int offset, final int removeLength,
            final String newText) {
        ErlideScanner.replaceText(name, offset, removeLength, newText);
    }

    @Override
    public ErlToken getTokenAt(final int offset) {
        return ErlideScanner.getTokenAt(name, offset);
    }

    @Override
    public String getText() {
        return ErlideScanner.getText(name);
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public void addref() {
        ErlideScanner.addref(name);
    }
}
