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
package org.erlide.wrangler.refactoring.duplicatedcode;

import java.util.List;

import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.IDuplicatedCodeResultDisplayer;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeElement;

/**
 * Manages the duplicates views visibility through static methods
 * 
 * @author Gyorgy Orosz
 * 
 */
public class DuplicatesUIManager {
    /**
     * Duplicated view ID.
     */
    public static final String duplicatedView = "org.erlide.wrangler.refactoring.duplicatedcode.views.DuplicatedCodeView";
    // private static List<DuplicatedCode> result;
    private static IDuplicatedCodeResultDisplayer dupDisplayer;

    /**
     * Stores the view object
     * 
     * @param displayer
     *            duplicates view
     */
    public static void setDuplicatedCodeResultDisplayer(
            final IDuplicatedCodeResultDisplayer displayer) {
        dupDisplayer = displayer;
    }

    /**
     * Sets the duplicates in the view.
     * 
     * @param root
     *            list of the duplicates
     */
    public static void setRefactoringResults(
            final List<DuplicatedCodeElement> root) {
        dupDisplayer.showResult(root);
    }

    /**
     * Shows the duplicates view.
     */
    public static void showDuplicatesView() {
        final IWorkbench workbench = PlatformUI.getWorkbench();

        final IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
        try {

            @SuppressWarnings("unused")
            final IViewPart view = window.getActivePage().showView(
                    duplicatedView);

        } catch (final PartInitException e) {
            e.printStackTrace();
        }
    }

    /**
     * Hide the duplicates view.
     */
    public static void closeDuplicatesView() {
        final IWorkbench workbench = PlatformUI.getWorkbench();

        final IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
        IViewPart view;
        try {
            view = window.getActivePage().showView(duplicatedView);
            window.getActivePage().hideView(view);
        } catch (final PartInitException e) {
            e.printStackTrace();
        }

    }
}
