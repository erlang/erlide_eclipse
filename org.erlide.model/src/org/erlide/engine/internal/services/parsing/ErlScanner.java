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
package org.erlide.engine.internal.services.parsing;

import org.erlide.engine.services.parsing.ErlToken;
import org.erlide.engine.services.parsing.ScannerService;
import org.erlide.runtime.api.IOtpRpc;

/**
 * Erlang syntax scanner
 */
public class ErlScanner implements ScannerService {
    private final String name;
    private final ErlideScanner scanner;

    public ErlScanner(final IOtpRpc backend, final String name) {
        this.name = name;
        scanner = new ErlideScanner(backend);
        scanner.create(name);
    }

    @Override
    public void initialScan(final String initialText, final String path,
            final boolean logging) {
        final String pathNotNull = path == null ? "" : path;
        scanner.initialScan(name, pathNotNull, initialText);
    }

    @Override
    public void dispose() {
        scanner.dispose(name);
    }

    @Override
    public void replaceText(final int offset, final int removeLength, final String newText) {
        scanner.replaceText(name, offset, removeLength, newText);
    }

    @Override
    public ErlToken getTokenAt(final int offset) {
        return scanner.getTokenAt(name, offset);
    }

    @Override
    public void addref() {
        scanner.addref(name);
    }

}
