package org.erlide.core.services.builder;

import java.util.concurrent.ConcurrentLinkedQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.google.common.collect.Queues;

public class BuildQueueProcessor extends Job {
    private static BuildQueueProcessor instance;
    private final ConcurrentLinkedQueue<BuildWorkerInfo> queue = Queues
            .newConcurrentLinkedQueue();
    private volatile boolean stopped;

    public BuildQueueProcessor(final String name) {
        super(name);
        setSystem(true);
        setPriority(Job.DECORATE);
        schedule();
    }

    public void addWork(final BuildWorkerInfo work) {
        queue.add(work);
        schedule(100);
    }

    @Override
    protected IStatus run(final IProgressMonitor monitor) {
        BuildWorkerInfo work = null;
        while (!stopped) {
            work = queue.poll();
            if (work == null) {
                try {
                    Thread.sleep(100);
                } catch (final InterruptedException e) {
                    e.printStackTrace();
                }
                continue;
            }
            MarkerUtils.createTaskMarkers(work.project, work.resource);
        }
        return Status.OK_STATUS;
    }

    public static BuildQueueProcessor getInstance() {
        if (instance == null) {
            instance = new BuildQueueProcessor("tasks");
        }
        return instance;
    }

    public void stop() {
        stopped = true;
    }
}
