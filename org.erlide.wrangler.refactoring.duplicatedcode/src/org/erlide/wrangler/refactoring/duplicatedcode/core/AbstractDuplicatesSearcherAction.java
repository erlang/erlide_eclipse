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
package org.erlide.wrangler.refactoring.duplicatedcode.core;

import java.io.IOException;
import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.PlatformUI;
import org.erlide.wrangler.refactoring.core.exception.WranglerWarningException;
import org.erlide.wrangler.refactoring.duplicatedcode.DuplicatesUIManager;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeElement;
import org.erlide.wrangler.refactoring.exception.WranglerRpcParsingException;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

/**
 * Abstract class, which has common methods for running a duplicated search
 * refactoring
 * 
 * @author Gyorgy Orosz
 * 
 */
public abstract class AbstractDuplicatesSearcherAction extends AbstractHandler {

    @Override
    public Object execute(final ExecutionEvent event) throws ExecutionException {
        run();
        return null;
    }

    protected final String rpcErrorMsg = "An error occured during the refactoring!";

    /**
     * Runs the refactoring.
     */
    public void run() {
        // TODO: run it in a new thread
        selectionChanged();
        if (getUserInput()) {
            final IProgressMonitor monitor = new NullProgressMonitor();
            try {
                IResultParser result;
                monitor.beginTask("Detecting..", 0);

                result = callRefactoring();
                if (result.isSuccessful()) {
                    showDuplicatesView();
                    addDuplicates(result.getDuplicates());
                } else {
                    DuplicatesUIManager.closeDuplicatesView();
                    displayErrorNotification(result.getErrorMessage());
                }
            } catch (final WranglerWarningException e) {

            } catch (final WranglerRpcParsingException e) {
                displayErrorNotification(rpcErrorMsg);
            } catch (final CoreException e) {
                displayErrorNotification(rpcErrorMsg);
            } catch (final IOException e) {
                displayErrorNotification(rpcErrorMsg);
            } finally {
                monitor.done();
            }
        }

    }

    protected abstract boolean getUserInput();

    protected void addDuplicates(
            final List<DuplicatedCodeElement> duplicatedCode) {
        DuplicatesUIManager.setRefactoringResults(duplicatedCode);
    }

    protected abstract IResultParser callRefactoring()
            throws WranglerRpcParsingException, CoreException, IOException,
            WranglerWarningException;

    /**
     * Handles the event when a selectino is changed in the workbench
     */
    public void selectionChanged() {
        GlobalParameters.setEditor(PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage().getActiveEditor());
    }

    /*
     * public void setActiveEditor(IAction action, IEditorPart targetEditor) {
     * GlobalParameters.setEditor(targetEditor); }
     */

    void displayErrorNotification(final String errorMsg) {
        MessageDialog.openError(PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getShell(), "Refactoring error",
                errorMsg);

    }

    void showDuplicatesView() {
        DuplicatesUIManager.showDuplicatesView();
    }

}
