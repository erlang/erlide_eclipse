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
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.rules.FastPartitioner;
import org.eclipse.jface.text.rules.IPartitionTokenScanner;
import org.eclipse.swt.widgets.Display;
import org.erlide.backend.console.BackendShellListener;
import org.erlide.backend.console.IBackendShell;
import org.erlide.backend.console.IoRequest.IoRequestKind;

public final class ErlConsoleDocument extends Document implements
        BackendShellListener {

    private static String[] LEGAL_CONTENT_TYPES = null;

    private final IBackendShell shell;

    public ErlConsoleDocument(final IBackendShell shell) {
        super();

        if (LEGAL_CONTENT_TYPES == null) {
            final IoRequestKind[] values = IoRequestKind.values();
            final String[] ss = new String[values.length];
            for (int i = 0; i < values.length; i++) {
                ss[i] = values[i].name();
            }
            LEGAL_CONTENT_TYPES = ss;
        }

        Assert.isNotNull(shell);
        this.shell = shell;
        shell.addListener(this);
        changed(shell);

        final IDocumentPartitioner partitioner = new FastPartitioner(
                createScanner(), LEGAL_CONTENT_TYPES);
        partitioner.connect(this);
        setDocumentPartitioner(partitioner);
    }

    private IPartitionTokenScanner createScanner() {
        return new IoRequestScanner(shell);
    }

    @Override
    public void changed(final IBackendShell aShell) {
        if (aShell != shell) {
            return;
        }
        final String text = shell.getText();
        Display.getDefault().asyncExec(new Runnable() {

            @Override
            public void run() {
                try {
                    replace(0, getLength(), text);
                } catch (final BadLocationException e) {
                    e.printStackTrace();
                }
            }
        });
    }

    public IBackendShell getShell() {
        return shell;
    }
}
