package org.erlide.shade.bterl.ui.launcher;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
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
import org.erlide.jinterface.ErlLogger;
import org.erlide.test_support.popup.actions.OpenResultsJob;
import org.erlide.test_support.ui.suites.RegressionResultsView;

public class RegressionLauncher {
    private static volatile RegressionLauncher instance;

    final String watcherThreadName = "bterl regression watcher";

    private volatile Thread watcherThread = null;
    private final Object watcherLock = new Object();

    private RegressionResultsView review;

    public static RegressionLauncher getInstance() {
        if (instance == null) {
            instance = new RegressionLauncher();
        }
        return instance;
    }

    private RegressionLauncher() {
    }

    public void launch(final String dir, final IProgressMonitor monitor,
            final RegressionResultsView rview) {
        review = rview;
        initResultsView(dir, rview);

        // TODO queue requests for different directories? we need a manager...
        if (isAlreadyRunning()) {
            // TODO feedback to user!
            System.out.println("!!!!!!!!!!!! regression already running");
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

            try {
                final Thread thread = new Thread(new ListenerRunnable(is,
                        monitor, rview));
                thread.setDaemon(true);
                thread.setName("bterl regression listener");
                thread.start();

                synchronized (watcherLock) {
                    watcherThread = new Thread(new WatcherRunnable(container,
                            cmake));
                    watcherThread.setDaemon(true);
                    watcherThread.setName(watcherThreadName);
                    watcherThread.start();
                }
            } finally {
                try {
                    is.close();
                } catch (final IOException e) {
                    e.printStackTrace();
                }
            }
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

    private void initResultsView(final String dir,
            final RegressionResultsView rview) {
        if (rview != null) {
            rview.clear();
            rview.setMessage("Launching regression for: " + dir);
        }
    }

    private boolean isAlreadyRunning() {
        return watcherThread != null;
    }

    private final class WatcherRunnable implements Runnable {
        private final IContainer container;
        private final Process cmake;

        WatcherRunnable(final IContainer container, final Process cmake) {
            this.container = container;
            this.cmake = cmake;
        }

        @Override
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
                synchronized (watcherLock) {
                    watcherThread = null;
                }
            } catch (final CoreException e) {
                // ignore
                e.printStackTrace();
            }
        }
    }

    private static final class ListenerRunnable implements Runnable {
        private final BufferedReader is;
        private final IProgressMonitor monitor;
        private final RegressionResultsView rview;

        ListenerRunnable(final BufferedReader is,
                final IProgressMonitor monitor,
                final RegressionResultsView rview) {
            this.is = is;
            this.monitor = monitor;
            this.rview = rview;
        }

        @Override
        public void run() {
            String line;
            try {
                while ((line = is.readLine()) != null) {
                    if (monitor != null) {
                        monitor.worked(1);
                    }
                    rview.addLine(line);
                }
            } catch (final IOException e) {
                e.printStackTrace();
            }
            monitor.done();
        }
    }

}
