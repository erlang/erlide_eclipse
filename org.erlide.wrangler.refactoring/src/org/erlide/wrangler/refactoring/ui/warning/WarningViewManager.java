/*******************************************************************************
 * Copyright (c) 2010 György Orosz.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     György Orosz - initial API and implementation
 ******************************************************************************/
package org.erlide.wrangler.refactoring.ui.warning;

import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

/**
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class WarningViewManager {
    /**
     * Warning view ID
     */
    public static final String warningViewID = WarningView.ID;

    private static IWarningHandler view;

    /**
     * Sets the warning view
     * 
     * @param _view
     *            Warning message view
     */
    public static void setWarningView(IWarningHandler _view) {
        view = _view;
    }

    /**
     * Adds a warning message to the view
     * 
     * @param message
     *            warning message
     */
    public static void addWarningMessage(String message) {
        try {
            // if (view == null) {
            setWarningView((IWarningHandler) showWarningView());
            // }
            view.addMessage(message);
            view.refresh();

        } catch (Exception t) {
            t.printStackTrace();
        }

    }

    /**
     * Shows the warning view.
     * 
     * @return view which is shown
     */
    public static IViewPart showWarningView() {
        IWorkbench workbench = PlatformUI.getWorkbench();

        IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
        try {
            IViewPart theView = window.getActivePage().showView(warningViewID);
            return theView;
        } catch (PartInitException e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * Hides the warning view.
     */
    public static void closeWarningView() {
        IWorkbench workbench = PlatformUI.getWorkbench();

        IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
        IViewPart theView = window.getActivePage().findView(warningViewID);
        window.getActivePage().hideView(theView);

    }
}
