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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.rules.FastPartitioner;
import org.eclipse.jface.text.rules.IPartitionTokenScanner;
import org.erlide.runtime.shell.BackendShellEvent;
import org.erlide.runtime.shell.BackendShellListener;
import org.erlide.runtime.shell.IBackendShell;
import org.erlide.runtime.shell.IoRequest.IoRequestKind;
import org.erlide.ui.util.DisplayUtils;
import org.erlide.util.ErlLogger;

public final class ErlConsoleDocument extends Document implements BackendShellListener {

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

        assertThat(shell, is(not(nullValue())));
        this.shell = shell;
        shell.addListener(this);
        changed(new BackendShellEvent(0, 0, get()));
        final IDocumentPartitioner partitioner = new FastPartitioner(createScanner(),
                LEGAL_CONTENT_TYPES);
        partitioner.connect(this);
        setDocumentPartitioner(partitioner);
    }

    private IPartitionTokenScanner createScanner() {
        return new IoRequestScanner(shell);
    }

    @Override
    public void changed(final BackendShellEvent event) {
        DisplayUtils.asyncExec(new Runnable() {

            @Override
            public void run() {
                try {
                    replace(event.getOffset(), event.getRemovedLength(), event.getText());
                } catch (final BadLocationException e) {
                    ErlLogger.error(e);
                }
            }
        });
    }

    public IBackendShell getShell() {
        return shell;
    }
}
