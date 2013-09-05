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
import org.erlide.model.services.scanner.ScannerService;

/**
 * Erlang syntax scanner
 */
public class ErlScanner implements IErlScanner {
    private final String name;
    private final ScannerService scannerService;

    public ErlScanner(final String name) {
        this.name = name;
        scannerService = ErlangEngine.getInstance().getScannerService();
        scannerService.create(name);
    }

    @Override
    public void initialScan(final String initialText, final String path,
            final boolean logging) {
        final String pathNotNull = path == null ? "" : path;
        scannerService.initialScan(name, pathNotNull, initialText, logging);
    }

    @Override
    public void dispose() {
        scannerService.dispose(name);
    }

    @Override
    public void replaceText(final int offset, final int removeLength,
            final String newText) {
        scannerService.replaceText(name, offset, removeLength, newText);
    }

    @Override
    public ErlToken getTokenAt(final int offset) {
        return scannerService.getTokenAt(name, offset);
    }

    @Override
    public String getText() {
        return scannerService.getText(name);
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public void addref() {
        scannerService.addref(name);
    }
}
