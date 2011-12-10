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
package org.erlide.wrangler.refactoring.ui.dnd;

import java.util.ArrayList;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.util.LocalSelectionTransfer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.ltk.ui.refactoring.RefactoringWizard;
import org.eclipse.ltk.ui.refactoring.RefactoringWizardOpenOperation;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.navigator.CommonDropAdapter;
import org.erlide.core.model.erlang.IErlFunctionClause;
import org.erlide.core.model.root.IErlElement;
import org.erlide.ui.navigator.dnd.INavigatorDropHandler;
import org.erlide.wrangler.refactoring.core.internal.MoveFunctionRefactoring;
import org.erlide.wrangler.refactoring.exception.WranglerException;
import org.erlide.wrangler.refactoring.ui.wizard.DefaultWranglerRefactoringWizard;
import org.erlide.wrangler.refactoring.ui.wizardpages.WranglerPage;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

/**
 * Implements san erlide drag'n'drop extension point
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class MoveFunctionDropHandler implements INavigatorDropHandler {

    /**
     * Default constructor
     */
    public MoveFunctionDropHandler() {
    }

    @Override
    public IStatus validateDrop(final Object target, final int operation,
            final TransferData transferType) {
        final ISelection sel = (ISelection) LocalSelectionTransfer
                .getTransfer().nativeToJava(transferType);
        final TreeSelection s = (TreeSelection) sel;
        final IErlElement e = (IErlElement) s.getFirstElement();

        if (e instanceof IErlFunctionClause) {
            if (target instanceof IErlElement || target instanceof IFile) {
                return Status.OK_STATUS;
            }
        }
        return Status.CANCEL_STATUS;
    }

    @Override
    public IStatus handleDrop(final CommonDropAdapter dropAdapter,
            final DropTargetEvent dropTargetEvent, final Object target) {

        // get the source data
        final TransferData td = dropAdapter.getCurrentTransfer();
        final ISelection sel = (ISelection) LocalSelectionTransfer
                .getTransfer().nativeToJava(td);
        final TreeSelection s = (TreeSelection) sel;
        try {
            GlobalParameters.setSelection(s);
        } catch (final WranglerException e1) {
            e1.printStackTrace();
        }

        // get the target data
        String moduleName;
        IFile file;
        if (target instanceof IFile) {
            file = (IFile) target;
        } else {
            file = (IFile) ((IErlElement) target).getResource();
        }
        moduleName = file.getName();

        moduleName = moduleName.substring(0, moduleName.lastIndexOf("."));

        final MoveFunctionRefactoring refactoring = new MoveFunctionRefactoring();
        refactoring.setUserInput(moduleName);
        final RefactoringWizard wizard = new DefaultWranglerRefactoringWizard(
                refactoring, RefactoringWizard.DIALOG_BASED_USER_INTERFACE,
                new ArrayList<WranglerPage>());

        final Shell shell = PlatformUI.getWorkbench().getDisplay()
                .getActiveShell();

        final RefactoringWizardOpenOperation op = new RefactoringWizardOpenOperation(
                wizard);

        try {
            op.run(shell, refactoring.getName());
        } catch (final Exception e) {
            e.printStackTrace();
        }

        System.out.print("hand");

        return Status.OK_STATUS;
    }
}
