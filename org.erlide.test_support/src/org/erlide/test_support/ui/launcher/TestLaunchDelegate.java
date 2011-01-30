package org.erlide.test_support.ui.launcher;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.RuntimeVersion;
import org.erlide.jinterface.backend.events.EventHandler;
import org.erlide.jinterface.backend.util.PreferencesUtils;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.jinterface.util.ErlUtils;
import org.erlide.jinterface.util.ParserException;
import org.erlide.runtime.backend.RuntimeInfoManager;
import org.erlide.runtime.debug.ErlDebugConstants;
import org.erlide.runtime.launch.ErlLaunchAttributes;
import org.erlide.runtime.launch.ErlangDebugHelper;
import org.erlide.runtime.launch.ErlangLaunchDelegate;
import org.erlide.test_support.Activator;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlang;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangChar;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.SignatureException;
import com.google.common.base.Joiner;
import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

public class TestLaunchDelegate extends ErlangLaunchDelegate {

    private String testcase;
    private String suite;
    private String mode;
    private String project;
    private File workdir;

    @Override
    public void launch(final ILaunchConfiguration cfg, final String amode,
            final ILaunch launch, final IProgressMonitor monitor)
            throws CoreException {

        project = cfg.getAttribute(TestLaunchAttributes.PROJECT, "");
        mode = ILaunchManager.DEBUG_MODE.equals(amode) ? amode : cfg
                .getAttribute(TestLaunchAttributes.MODE, "");
        final String wdir = cfg.getAttribute(TestLaunchAttributes.WORKDIR, "");
        suite = cfg.getAttribute(TestLaunchAttributes.SUITE, "");
        testcase = cfg.getAttribute(TestLaunchAttributes.CASE, "");

        workdir = new File(wdir);
        if ("regression".equals(mode)) {
            RegressionLauncher.getInstance().launch(wdir, monitor);
        } else {
            doLaunch(cfg, launch, monitor);
        }
    }

    private void doLaunch(final ILaunchConfiguration config,
            final ILaunch launch, final IProgressMonitor monitor)
            throws CoreException {

        System.out.println("---@> launch " + workdir.getAbsolutePath() + " -> "
                + suite + ":" + testcase + " (" + mode + ")");
        if (!workdir.exists()) {
            ErlLogger.warn(
                    "Attempting to start bterl tests in missing directory %s",
                    workdir.getAbsolutePath());
            return;
        }
        if ("regression".equals(mode)) {
            // regression is handled elsewhere
            return;
        }
        runMakeLinks(monitor);

        final ILaunchConfiguration cfg = setupConfiguration(config, project,
                workdir);

        final String amode = ILaunchManager.DEBUG_MODE.equals(mode) ? ILaunchManager.DEBUG_MODE
                : ILaunchManager.RUN_MODE;
        super.doLaunch(cfg, amode, launch, true, null);

        // FIXME if (backend == null) {
        // return null;
        // }
        //
        // initDebugger(monitor, backend);
        // startMonitorJob(monitor, backend);
        //
        // monitor.worked(1);
        //
        // return backend;
    }

    private void runMakeLinks(final IProgressMonitor monitor) {
        try {
            monitor.worked(1);
            ErlLogger.debug("running make_links");
            final Process makeLinks = DebugPlugin.exec(
                    new String[] { workdir.getAbsolutePath() + "/make_links" },
                    workdir);
            while (true) {
                try {
                    makeLinks.waitFor();
                    break;
                } catch (final InterruptedException e1) {
                }
            }
            ErlLogger.debug("make_links done");
            final IWorkspaceRoot wroot = ResourcesPlugin.getWorkspace()
                    .getRoot();
            try {
                final IFile r = wroot.findFilesForLocationURI(new URI("file://"
                        + workdir.getAbsolutePath()))[0];
                r.refreshLocal(IResource.DEPTH_ONE, null);
            } catch (final Exception e) {
                e.printStackTrace();
            }

        } catch (final CoreException e) {
            e.printStackTrace();
            // there is no make_links, ignore
        }
    }

    private void initDebugger(final IProgressMonitor monitor,
            final Backend backend) {

        // TODO do we do this like this?

        final EventHandler handler = new EventHandler() {
            @Override
            protected void doHandleMsg(final OtpErlangObject msg)
                    throws Exception {
                final OtpErlangObject event = getStandardEvent(msg,
                        "bterl_debugger");
                if (event == null) {
                    return;
                }
                final String[] modules = workdir.list(new FilenameFilter() {
                    public boolean accept(final File dir, final String filename) {
                        return filename.endsWith(".erl");
                    }
                });
                for (final String pm : modules) {
                    System.out.println("reinterpret:: " + pm);
                    getDebugHelper()
                            .interpret(backend, project, pm, true, true);
                }
                // getDebugTarget().installDeferredBreakpoints();

                final OtpErlangPid pid = (OtpErlangPid) event;
                backend.send(pid, new OtpErlangAtom("ok"));
                backend.getEventDaemon().removeHandler(this);
            }
        };
        backend.getEventDaemon().addHandler(handler);
    }

    private void startMonitorJob(final IProgressMonitor monitor,
            final Backend backend) {

        // TODO do this in a job

        final EventHandler handler = new EventHandler() {
            @Override
            protected void doHandleMsg(final OtpErlangObject msg)
                    throws Exception {
                final OtpErlangObject event = getStandardEvent(msg,
                        "bterl_monitor");
                if (event == null) {
                    return;
                }
                // TODO check events and do something
            }
        };
        backend.getEventDaemon().addHandler(handler);
    }

    private OtpErlangList getFlags(final String theMode) {
        final List<OtpErlangObject> e_flags = Lists.newArrayList();
        if ("cover".equals(theMode)) {
            e_flags.add(new OtpErlangChar('c'));
        }
        if ("trace".equals(theMode)) {
            e_flags.add(new OtpErlangChar('t'));
        }
        if ("debug".equals(theMode)) {
            // we start debugger ourselves
            // e_flags.add(new OtpErlangChar('d'));
        }
        return OtpErlang.mkList(e_flags);
    }

    private ILaunchConfiguration setupConfiguration(
            final ILaunchConfiguration config, final String project,
            final File workdir) throws CoreException {
        final ILaunchConfigurationWorkingCopy wc = config.getWorkingCopy();

        final boolean vobBterl = "false".equals(System.getProperty(
                "shade.bterl.local", "true"));
        System.out.println(".... vobBterl: " + vobBterl);

        final String bterlPath = getBterlPath();
        System.out.println("... internal path = " + bterlPath);

        final String runtimeName = RuntimeInfoManager.getDefault()
                .getRuntime(new RuntimeVersion("R13B"), "").getName();

        final List<String> paths = Lists.newArrayList();
        final String workdirPath = getOSIndependentPath(workdir);
        paths.add(workdirPath);
        paths.add(bterlPath);

        wc.setAttribute(ErlLaunchAttributes.RUNTIME_NAME, runtimeName);
        // FIXME: unique node name!
        wc.setAttribute(ErlLaunchAttributes.NODE_NAME,
                "bterl" + System.currentTimeMillis());
        wc.setAttribute(ErlLaunchAttributes.START_ME, true);
        wc.setAttribute(ErlLaunchAttributes.WORKING_DIR, workdirPath);
        wc.setAttribute(ErlLaunchAttributes.CONSOLE, true);
        wc.setAttribute(ErlLaunchAttributes.COOKIE, "shade");
        wc.setAttribute(ErlLaunchAttributes.USE_LONG_NAME, false);
        final String args = "-boot start_clean -sasl sasl_error_logger false -pa "
                + PreferencesUtils.packList(paths, " -pa ");
        wc.setAttribute(ErlLaunchAttributes.EXTRA_ARGS, args);

        wc.setAttribute(ErlLaunchAttributes.PROJECTS, project);
        final List<String> modules = Lists.newArrayList();
        for (final String m : workdir.list(new FilenameFilter() {
            public boolean accept(final File dir, final String name) {
                return name.endsWith(".erl");
            }
        })) {
            modules.add(project + ":" + m);
        }
        wc.setAttribute(ErlLaunchAttributes.DEBUG_INTERPRET_MODULES, modules);
        wc.setAttribute(ErlLaunchAttributes.DEBUG_FLAGS,
                ErlDebugConstants.ATTACH_ON_BREAKPOINT);

        final OtpErlangString e_dir = new OtpErlangString(workdirPath);
        final OtpErlangAtom e_suite = new OtpErlangAtom(suite);
        final OtpErlangAtom e_test = new OtpErlangAtom(testcase);
        final OtpErlangList e_flags = getFlags(mode);

        final OtpErlangTuple trace = OtpErlang.mkTuple(
                new OtpErlangString("tr.txt"),
                "trace".equals(mode) ? OtpErlang.mkList(new OtpErlangString(
                        ".*")) : new OtpErlangList());
        final OtpErlangTuple cmd = OtpErlang.mkTuple(e_dir, e_suite, e_test);
        final String cb = ILaunchManager.DEBUG_MODE.equals(mode) ? "{bterl_watcher, init_debugger}"
                : "undefined";
        final String s = e_flags.toString() + "," + cmd.toString() + ","
                + trace.toString() + "," + cb;
        System.out.println(">>>>  * bt_run:start(" + s + ")");

        wc.setAttribute(ErlLaunchAttributes.MODULE, "bterl_watcher");
        wc.setAttribute(ErlLaunchAttributes.FUNCTION, "start_bterl");
        try {
            final String args2 = ErlUtils.format("{~x,~s,~x,~x,~s}", e_flags,
                    cmd, trace, cb, workdirPath).toString();
            wc.setAttribute(ErlLaunchAttributes.ARGUMENTS, args2);
        } catch (final ParserException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (final SignatureException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        wc.setModes(Sets.newHashSet(ILaunchManager.DEBUG_MODE,
                ILaunchManager.RUN_MODE));

        return wc;
    }

    private static String getOSIndependentPath(final File dir) {
        final String path = dir.getAbsolutePath();
        final Iterable<String> spath = Splitter.on('\\').split(path);
        return Joiner.on('/').join(spath);
    }

    private String getBterlPath() throws CoreException {
        String bterlPath;
        try {
            bterlPath = getPluginPath().append("ebin").toString();
            // FIXME use real bterl path
            bterlPath = "/proj/uz/shade/erlang_bt_tool/ebin";
        } catch (final IOException e) {
            e.printStackTrace();
            throw new CoreException(new Status(IStatus.ERROR,
                    Activator.PLUGIN_ID, "Could not access internal bterl"));
        }
        return bterlPath;
    }

    private static Path getPluginPath() throws IOException {
        final Bundle bundle = Activator.getDefault().getBundle();
        URL url = FileLocator.find(bundle, new Path(""), null);
        url = FileLocator.resolve(url);
        return new Path(url.getPath());
    }

    @SuppressWarnings("unused")
    private void doLaunchWithScript(final File theWorkdir, final String theSuite,
            final String theTestCase, final String theMode, final ILaunch launch,
            final IProgressMonitor monitor) {
        System.out.println("---@> launch " + theWorkdir.getAbsolutePath() + " -> "
                + theSuite + ":" + theTestCase + " (" + theMode + ")");
        if (!theWorkdir.exists()) {
            return;
        }
        final List<String> cmds = Lists.newArrayList();
        if ("regression".equals(theMode)) {
            cmds.add("clearmake");
            cmds.add("posttest");
        } else {
            cmds.add("bt_erl");
            if (theSuite.length() > 0) {
                cmds.add(theSuite);
                if (theTestCase.length() > 0) {
                    cmds.add(theTestCase);
                }
            }
            if (!theMode.equals("run")) {
                cmds.add("-" + theMode);
            }
        }
        final String[] cmdline = cmds.toArray(new String[cmds.size()]);
        System.out.println("---   cmdline (" + theWorkdir.getAbsolutePath()
                + ")= " + Arrays.toString(cmdline));
    }

    public ErlangDebugHelper getDebugHelper() {
        // FIXME use BterlLauncher instead!
        return new TestDebugHelper(workdir);
    }

}
