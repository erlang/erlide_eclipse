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

import org.eclipse.core.runtime.ListenerList;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.console.IConsoleConstants;
import org.eclipse.ui.console.IConsoleDocumentPartitioner;
import org.eclipse.ui.console.IConsoleView;
import org.eclipse.ui.console.TextConsole;
import org.eclipse.ui.part.IPageBookViewPage;
import org.erlide.backend.IBackend;
import org.erlide.backend.console.IBackendShell;

public class ErlangConsole extends TextConsole {
    private final IBackendShell shell;
    protected ListenerList consoleListeners;
    protected ErlangConsolePartitioner partitioner;
    private boolean stopped = false;
    private final IBackend backend;

    public ErlangConsole(final IBackend backend) {
        super(backend.getName(), null, null, true);
        this.backend = backend;

        shell = backend.getShell("main");
        consoleListeners = new ListenerList(ListenerList.IDENTITY);

        partitioner = new ErlangConsolePartitioner();
        getDocument().setDocumentPartitioner(partitioner);
        partitioner.connect(getDocument());
    }

    @Override
    public IPageBookViewPage createPage(final IConsoleView view) {
        return new ErlangConsolePage(view, this);
    }

    public IBackend getBackend() {
        return backend;
    }

    public IBackendShell getShell() {
        return shell;
    }

    @Override
    public ImageDescriptor getImageDescriptor() {
        return null;
    }

    @Override
    public String getName() {
        return "Erlang: " + backend.getName();
    }

    @Override
    public String getType() {
        return null;
    }

    @Override
    public void addPropertyChangeListener(final IPropertyChangeListener listener) {
    }

    @Override
    public void removePropertyChangeListener(
            final IPropertyChangeListener listener) {
    }

    public void show() {
        final IWorkbenchPage page = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();
        final String id = IConsoleConstants.ID_CONSOLE_VIEW;
        IConsoleView view;
        try {
            view = (IConsoleView) page.showView(id);
            view.display(this);
        } catch (final PartInitException e) {
            e.printStackTrace();
        }
    }

    @Override
    protected IConsoleDocumentPartitioner getPartitioner() {
        return partitioner;
    }

    public void stop() {
        stopped = true;
    }

    public boolean isStopped() {
        return stopped;
    }

}
