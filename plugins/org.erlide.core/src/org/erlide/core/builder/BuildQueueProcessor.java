package org.erlide.core.builder;

import java.util.Queue;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.erlide.engine.MarkerUtils;

import com.google.common.collect.Queues;

public class BuildQueueProcessor extends Job {
    private static BuildQueueProcessor instance;
    private final Queue<BuildWorkerInfo> queue = Queues
            .newConcurrentLinkedQueue();

    public BuildQueueProcessor(final String name) {
        super(name);
        setSystem(true);
        setPriority(Job.DECORATE);
    }

    public void addWork(final BuildWorkerInfo work) {
        queue.add(work);
        schedule(100);
    }

    @Override
    protected IStatus run(final IProgressMonitor monitor) {
        BuildWorkerInfo work = null;
        work = queue.poll();
        while (work != null) {
            MarkerUtils.removeTaskMarkers(work.resource);
            MarkerUtils.createTaskMarkers(work.resource);
            work = queue.poll();
        }
        return Status.OK_STATUS;
    }

    public static synchronized BuildQueueProcessor getInstance() {
        if (BuildQueueProcessor.instance == null) {
            BuildQueueProcessor.instance = new BuildQueueProcessor("tasks");
        }
        return BuildQueueProcessor.instance;
    }

}
