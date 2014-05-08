/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.backend;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.erlide.runtime.epmd.EpmdWatcher;

/**
 * Periodically, query epmd to see if there are any new nodes that have been
 * registered.
 *
 */
public class EpmdWatchJob extends Job {

    private static final int DEFAULT_INTERVAL = 1000;

    private int interval = DEFAULT_INTERVAL;
    private final EpmdWatcher watcher;
    private boolean isStopped;

    public EpmdWatchJob(final EpmdWatcher aWatcher, final int anInterval) {
        super("Checking EPMD for new backends");
        assert aWatcher != null;

        watcher = aWatcher;
        interval = anInterval < 0 ? DEFAULT_INTERVAL : anInterval;
        isStopped = false;

        setSystem(true);
        setPriority(SHORT);
    }

    public EpmdWatchJob(final EpmdWatcher watcher) {
        this(watcher, DEFAULT_INTERVAL);
    }

    @Override
    protected IStatus run(final IProgressMonitor monitor) {

        watcher.checkEpmd();

        if (!isStopped) {
            this.schedule(interval);
        }
        return Status.OK_STATUS;
    }

    public void setInterval(final int interval) {
        this.interval = interval;
    }

    public int getInterval() {
        return interval;
    }

    public void stop() {
        isStopped = true;
    }

}
