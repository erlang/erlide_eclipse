/*******************************************************************************
 * Copyright (c) 2006 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.erlide.ui.util.SFProjectSupport;

public class BugReportAction implements IWorkbenchWindowActionDelegate {

    public void dispose() {
    }

    public void init(final IWorkbenchWindow window) {
    }

    public void run(final IAction action) {
        SFProjectSupport.openBugsReport();
    }

    public void selectionChanged(final IAction action,
            final ISelection selection) {
    }

}
