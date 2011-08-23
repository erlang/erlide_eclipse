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
package org.erlide.wrangler.refactoring.core;

import java.io.IOException;
import java.util.ArrayList;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.Refactoring;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.erlide.wrangler.refactoring.Activator;
import org.erlide.wrangler.refactoring.backend.ChangedFile;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.util.WranglerUtils;

/**
 * Abstract class for implementing Wrangler refactorings. Implementors should
 * extend this.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public abstract class WranglerRefactoring extends Refactoring {
    protected ArrayList<ChangedFile> changedFiles;

    /**
     * @return the changed files by the refactoring
     */
    public ArrayList<ChangedFile> getChangedFiles() {
        return changedFiles;
    }

    /**
     * Run the RPC call. Usually only one RPC call is needed, for this, this
     * function is used to do the trick.
     * 
     * @param sel
     *            selected code piece
     * @return parsed refactoring message
     */
    public abstract IRefactoringRpcMessage run(IErlSelection sel);

    @Override
    public abstract RefactoringStatus checkFinalConditions(IProgressMonitor pm)
            throws CoreException, OperationCanceledException;

    @Override
    public abstract RefactoringStatus checkInitialConditions(IProgressMonitor pm)
            throws CoreException, OperationCanceledException;

    @Override
    public Change createChange(final IProgressMonitor pm) throws CoreException,
            OperationCanceledException {
        pm.beginTask("Creating changes", changedFiles.size() + 1);
        final CompositeChange change = new CompositeChange(getName());
        pm.internalWorked(1);

        try {
            Change c;
            for (final ChangedFile e : changedFiles) {
                c = e.createChanges();
                if (c != null) {
                    change.add(c);
                    pm.internalWorked(1);
                }
            }
        } catch (final IOException e) {
            final Status s = new Status(IStatus.ERROR, Activator.PLUGIN_ID,
                    e.getMessage());

            throw new CoreException(s);
        } finally {
            pm.done();
        }

        return change;
    }

    @Override
    public abstract String getName();

    /**
     * This operation is run after doing the refactoring.
     */
    public void doAfterRefactoring() {
        WranglerUtils.notifyErlide(getChangedFiles());
    }

    /**
     * This operation is run before the refactoring is started.
     * 
     */
    public void doBeforeRefactoring() {

    }

    /**
     * Obtains default value of refactored element
     * 
     * @return default (previous) value of refactored element
     */
    public String getDefaultValue() {
        return "";
    }

}
