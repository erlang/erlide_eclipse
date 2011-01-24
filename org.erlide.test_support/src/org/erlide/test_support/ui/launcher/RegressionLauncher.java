package org.erlide.test_support.ui.launcher;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.management.ManagementFactory;
import java.net.URI;
import java.net.URISyntaxException;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.debug.core.DebugPlugin;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.test_support.popup.actions.OpenResultsJob;

public class RegressionLauncher {
    static final String watcherThreadName = "bterl regression watcher";

    static public void doLaunchRegression(final String dir,
            final IProgressMonitor monitor) {
        // TODO queue requests for different directories? we need a manager...
        if (isAlreadyRunning()) {
            // TODO any feedback to user?
            return;
        }
        try {
            final URI uri = new URI("file://" + dir);
            final IContainer container = ResourcesPlugin.getWorkspace()
                    .getRoot().findContainersForLocationURI(uri)[0];
            final File workdir = new File(container.getLocation().toString());
            ErlLogger.debug("running 'make posttest' in "
                    + workdir.getAbsolutePath());
            final Process cmake = DebugPlugin.exec(new String[] { "clearmake",
                    "posttest" }, workdir);
            final InputStream in = cmake.getInputStream();
            final BufferedReader is = new BufferedReader(new InputStreamReader(
                    in));

            Thread thread = new Thread(new ListenerRunnable(is, monitor));
            thread.setDaemon(true);
            thread.setName("bterl regression listener");
            thread.start();

            thread = new Thread(new WatcherRunnable(container, cmake));
            thread.setDaemon(true);
            thread.setName(watcherThreadName);
            thread.start();
        } catch (final CoreException e) {
            // there is no make_links
            System.out.println("error running regression tests: "
                    + e.getMessage());
            e.printStackTrace();
        } catch (final URISyntaxException e) {
            // should not happen
            e.printStackTrace();
        }
    }

    private static boolean isAlreadyRunning() {
        int n = ManagementFactory.getThreadMXBean().getThreadCount();
        Thread[] tarray = new Thread[n];
        System.out.println(n);
        n = Thread.enumerate(tarray);
        System.out.println(n);
        for (int i = 0; i < n; i++) {
            if (watcherThreadName.equals(tarray[i].getName())) {
                return true;
            }
        }
        tarray = null;
        return false;
    }

    private static final class WatcherRunnable implements Runnable {
        private final IContainer container;
        private final Process cmake;

        WatcherRunnable(final IContainer container, final Process cmake) {
            this.container = container;
            this.cmake = cmake;
        }

        public void run() {
            while (true) {
                try {
                    cmake.waitFor();
                    break;
                } catch (final InterruptedException e1) {
                }
            }
            System.out.println("finished regression");
            try {
                container.refreshLocal(IResource.DEPTH_INFINITE,
                        new NullProgressMonitor());
                final IPath loc = container.getProjectRelativePath();
                final IPath p = loc.append("/do3/bttest/regression.html");
                final IFile log = container.getProject().getFile(p);
                final OpenResultsJob job = new OpenResultsJob(
                        "bterl.regression.report", log, null);
                job.schedule();
            } catch (final CoreException e) {
                // ignore
                e.printStackTrace();
            }
        }
    }

    private static final class ListenerRunnable implements Runnable {
        private final BufferedReader is;
        private final IProgressMonitor monitor;

        ListenerRunnable(final BufferedReader is, final IProgressMonitor monitor) {
            this.is = is;
            this.monitor = monitor;
        }

        public void run() {
            String line;
            try {
                while ((line = is.readLine()) != null) {
                    if (monitor != null) {
                        monitor.worked(1);
                    }
                    // TODO what to do with the output?
                    System.out.println(" >>>> " + line);
                }
            } catch (final IOException e) {
                e.printStackTrace();
            }
            monitor.done();
        }
    }

}
