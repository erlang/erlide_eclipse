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

import org.erlide.engine.ErlangEngine;
import org.erlide.model.erlang.ErlToken;
import org.erlide.model.erlang.IErlScanner;

/**
 * Erlang syntax scanner
 */
public class ErlScanner implements IErlScanner {
    private final String name;

    public ErlScanner(final String name) {
        this.name = name;
        ErlangEngine.getInstance().getScannerService().create(name);
    }

    @Override
    public void initialScan(final String initialText, final String path,
            final boolean logging) {
        final String pathNotNull = path == null ? "" : path;
        ErlangEngine.getInstance().getScannerService()
                .initialScan(name, pathNotNull, initialText, logging);
    }

    @Override
    public void dispose() {
        ErlangEngine.getInstance().getScannerService().dispose(name);
    }

    @Override
    public void replaceText(final int offset, final int removeLength,
            final String newText) {
        ErlangEngine.getInstance().getScannerService()
                .replaceText(name, offset, removeLength, newText);
    }

    @Override
    public ErlToken getTokenAt(final int offset) {
        return ErlangEngine.getInstance().getScannerService()
                .getTokenAt(name, offset);
    }

    @Override
    public String getText() {
        return ErlangEngine.getInstance().getScannerService().getText(name);
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public void addref() {
        ErlangEngine.getInstance().getScannerService().addref(name);
    }
}
