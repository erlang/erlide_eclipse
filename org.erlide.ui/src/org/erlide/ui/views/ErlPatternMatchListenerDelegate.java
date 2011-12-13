/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.views;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.ui.console.FileLink;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.ui.console.IHyperlink;
import org.eclipse.ui.console.IPatternMatchListenerDelegate;
import org.eclipse.ui.console.PatternMatchEvent;
import org.eclipse.ui.console.TextConsole;
import org.erlide.jinterface.ErlLogger;

public class ErlPatternMatchListenerDelegate implements
        IPatternMatchListenerDelegate {

    private TextConsole fConsole;

    @Override
    public void connect(final TextConsole console) {
        fConsole = console;
    }

    @Override
    public void disconnect() {
        fConsole = null;
    }

    @Override
    public void matchFound(final PatternMatchEvent event) {
        if (fConsole == null) {
            return;
        }
        try {
            final String txt = fConsole.getDocument().get(event.getOffset(),
                    event.getLength());
            final String[] v = txt.split(":");

            final IProject[] projects = ResourcesPlugin.getWorkspace()
                    .getRoot().getProjects();
            IResource res = null;
            for (final IProject prj : projects) {
                if (!prj.isOpen()) {
                    continue;
                }
                try {
                    res = recursiveFindNamedResourceWithReferences(prj, v[0]);
                    if (res != null) {
                        break;
                    }
                } catch (final CoreException e) {
                    ErlLogger.warn(e);
                }
            }
            IFile file = null;
            if (res != null && res instanceof IFile) {
                file = (IFile) res;
            }
            final IHyperlink link = new FileLink(file, null, -1, -1,
                    Integer.parseInt(v[1]));
            fConsole.addHyperlink(link, event.getOffset(), event.getLength());
        } catch (final BadLocationException e) {
            ErlLogger.warn(e);
        }
    }

    private static IResource recursiveFindNamedResourceWithReferences(
            final IContainer container, final String name) throws CoreException {
        final IResource r = recursiveFindNamedResource(container, name);
        if (r != null) {
            return r;
        }
        final IProject project = container.getProject();
        for (final IProject p : project.getReferencedProjects()) {
            final IResource r1 = recursiveFindNamedResource(p, name);
            if (r1 != null) {
                return r1;
            }
        }
        return null;
    }

    private static IResource recursiveFindNamedResource(
            final IContainer container, final String name) throws CoreException {
        if (!container.isAccessible()) {
            return null;
        }
        IResource r = container.findMember(name);
        if (r != null) {
            return r;
        }
        final IResource[] members = container.members();
        for (final IResource element : members) {
            r = element;
            if (r instanceof IContainer) {
                r = recursiveFindNamedResource((IContainer) r, name);
                if (r != null) {
                    return r;
                }
            }
        }
        return null;
    }

}
