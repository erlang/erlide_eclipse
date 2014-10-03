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
package org.erlide.ui.jinterface;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.progress.UIJob;
import org.erlide.backend.api.BackendException;
import org.erlide.runtime.rpc.RpcFuture;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.util.ErlLogger;

public abstract class AsyncCaller<T> implements Runnable {
    long interval;

    public AsyncCaller() {
        this(100);
    }

    public AsyncCaller(final long interval) {
        this.interval = interval;
    }

    protected abstract T prepare();

    protected abstract RpcFuture call() throws BackendException;

    protected abstract void handleResult(T context, RpcFuture result);

    @Override
    public void run() {
        final T context = prepare();
        try {
            final RpcFuture result = call();
            if (result == null) {
                return;
            }
            final Job job = new UIJob("async call updater") {
                @Override
                public IStatus runInUIThread(final IProgressMonitor monitor) {
                    try {
                        if (result.checkedGet(1, TimeUnit.MILLISECONDS) == null) {
                            schedule(interval);
                        }
                    } catch (final RpcException e) {
                        ErlLogger.error(e);
                    } catch (final TimeoutException e) {
                        ErlLogger.error(e);
                    }
                    handleResult(context, result);
                    if (monitor.isCanceled()) {
                        return Status.CANCEL_STATUS;
                    }
                    return new Status(IStatus.OK, ErlideUIPlugin.PLUGIN_ID, "done");
                }
            };
            job.schedule(interval);
        } catch (final BackendException e) {
            ErlLogger.error(e);
        }

    }

}
