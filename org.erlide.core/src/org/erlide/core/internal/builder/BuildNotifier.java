/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.internal.builder;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.osgi.util.NLS;
import org.erlide.core.builder.BuilderHelper;
import org.erlide.util.ErlLogger;

public class BuildNotifier {

    protected IProgressMonitor monitor;
    protected boolean cancelling;
    protected float percentComplete;
    protected float progressPerCompilationUnit;
    protected int workDone;
    protected int totalWork;
    protected String previousSubtask;

    public BuildNotifier(final IProgressMonitor monitor, final IProject project) {
        this.monitor = monitor;
        cancelling = false;
        workDone = 0;
        totalWork = 1000000;
    }

    /**
     * Notification before a compile that a unit is about to be compiled.
     */
    public void aboutToCompile(final IResource unit) {
        checkCancel();
        final String message = NLS.bind(BuilderMessages.build_compiling,
                unit.getFullPath());
        subTask(message);
        if (BuilderHelper.isDebugging()) {
            ErlLogger.debug(">>" + message);
        }
    }

    public void begin() {
        if (monitor != null) {
            monitor.beginTask("building", totalWork); //$NON-NLS-1$
        }
        previousSubtask = null;
    }

    /**
     * Check whether the build has been canceled.
     */
    public void checkCancel() {
        if (monitor != null && monitor.isCanceled()) {
            throw new OperationCanceledException();
        }
    }

    /**
     * Check whether the build has been canceled. Must use this call instead of
     * checkCancel() when within the compiler.
     */
    public void checkCancelWithinCompiler() {
        if (monitor != null && monitor.isCanceled() && !cancelling) {
            // Once the compiler has been canceled, don't check again.
            setCancelling(true);
            //
            // stop compiler
        }
    }

    /**
     * Notification while within a compile that a unit has finished being
     * compiled.
     */
    public void compiled(final IResource unit) {
        final String message = NLS.bind(BuilderMessages.build_compiling,
                unit.getFullPath());
        subTask(message);
        if (BuilderHelper.isDebugging()) {
            ErlLogger.debug("<<" + message);
        }
        updateProgressDelta(progressPerCompilationUnit);
        checkCancelWithinCompiler();
    }

    public void done() {
        updateProgress(1.0f);
        subTask(BuilderMessages.build_done);
        if (monitor != null) {
            monitor.done();
        }
        previousSubtask = null;
    }

    /**
     * Sets the cancelling flag, which indicates we are in the middle of being
     * cancelled.
     */
    public void setCancelling(final boolean cancelling) {
        this.cancelling = cancelling;
    }

    /**
     * Sets the amount of progress to report for compiling each compilation
     * unit.
     */
    public void setProgressPerCompilationUnit(final float progress) {
        progressPerCompilationUnit = progress;
    }

    public void subTask(final String message) {
        if (message.equals(previousSubtask)) {
            return; // avoid refreshing with same one
        }
        if (monitor != null) {
            monitor.subTask(message);
        }
        previousSubtask = message;
    }

    public void updateProgress(final float newPercentComplete) {
        if (newPercentComplete > percentComplete) {
            percentComplete = Math.min(newPercentComplete, 1.0f);
            final int work = Math.round(percentComplete * totalWork);
            if (work > workDone) {
                if (monitor != null) {
                    monitor.worked(work - workDone);
                }
                if (BuilderHelper.isDebugging()) {
                    ErlLogger.debug(java.text.NumberFormat.getPercentInstance().format(
                            percentComplete));
                }
                workDone = work;
            }
        }
    }

    public void updateProgressDelta(final float percentWorked) {
        updateProgress(percentComplete + percentWorked);
    }

    public boolean isCanceled() {
        if (monitor != null) {
            monitor.isCanceled();
        }
        return false;
    }

    public void worked(final int i) {
        if (monitor != null) {
            monitor.worked(i);
        }
    }

    public void beginTask(final String name, final int length) {
        if (monitor != null) {
            monitor.beginTask(name, length);
        }
    }

    public void doneTask() {
        if (monitor != null) {
            monitor.done();
        }
    }
}
