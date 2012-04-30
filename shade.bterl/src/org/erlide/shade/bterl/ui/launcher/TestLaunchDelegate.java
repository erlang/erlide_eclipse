package org.erlide.shade.bterl.ui.launcher;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.erlide.backend.BackendCore;
import org.erlide.backend.BackendData;
import org.erlide.backend.IBackend;
import org.erlide.backend.events.ErlangEventHandler;
import org.erlide.jinterface.ErlLogger;
import org.erlide.launch.ErlLaunchAttributes;
import org.erlide.launch.ErlangLaunchDelegate;
import org.erlide.launch.debug.ErlDebugConstants;
import org.erlide.shade.bterl.Activator;
import org.erlide.utils.ErlUtils;
import org.erlide.utils.ListsUtils;
import org.erlide.utils.TermParser;
import org.erlide.utils.TermParserException;
import org.osgi.framework.Bundle;
import org.osgi.service.event.Event;

import com.ericsson.otp.erlang.OtpErlang;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangChar;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.RuntimeVersion;
import com.ericsson.otp.erlang.SignatureException;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

public class TestLaunchDelegate extends ErlangLaunchDelegate {

    private static final OtpErlangAtom UNDEFINED = new OtpErlangAtom(
            "undefined");
    private static OtpErlangTuple BTERL_WATCHER_INIT_DEBUGGER;
    {
        try {
            BTERL_WATCHER_INIT_DEBUGGER = (OtpErlangTuple) TermParser
                    .getParser().parse("{bterl_watcher, init_debugger}");
        } catch (final TermParserException e) {
        }
    }

    private String testcase;
    private String suite;
    private String mode;
    private String projectName;
    private File workdir;
    private IProject project;

    @Override
    protected boolean preLaunch(final ILaunchConfiguration cfg,
            final String amode, final ILaunch launch,
            final IProgressMonitor monitor) throws CoreException {

        projectName = cfg.getAttribute(TestLaunchAttributes.PROJECT, "");
        project = ResourcesPlugin.getWorkspace().getRoot()
                .getProject(projectName);
        mode = ILaunchManager.DEBUG_MODE.equals(amode) ? amode : cfg
                .getAttribute(TestLaunchAttributes.MODE, "");
        final String wdir = cfg.getAttribute(TestLaunchAttributes.WORKDIR, "");
        suite = cfg.getAttribute(TestLaunchAttributes.SUITE, "");
        testcase = cfg.getAttribute(TestLaunchAttributes.CASE, "");

        workdir = new File(wdir);
        return true;
    }

    @Override
    protected IBackend doLaunch(final ILaunchConfiguration config,
            final String amode, final ILaunch launch,
            final IProgressMonitor monitor) throws CoreException {
        System.out.println("---@> launch " + workdir.getAbsolutePath() + " -> "
                + suite + ":" + testcase + " (" + mode + ")");
        if (!workdir.exists()) {
            ErlLogger.warn(
                    "Attempting to start bterl tests in missing directory %s",
                    workdir.getAbsolutePath());
            return null;
        }
        runMakeLinks(monitor);
        final ILaunchConfiguration cfg = setupConfiguration(config,
                projectName, workdir);
        final String theMode = ILaunchManager.DEBUG_MODE.equals(amode) ? ILaunchManager.DEBUG_MODE
                : ILaunchManager.RUN_MODE;
        return super.doLaunch(cfg, theMode, launch, monitor);
    }

    @Override
    protected BackendData configureBackend(final BackendData data,
            final ILaunchConfiguration config, final String aMode,
            final ILaunch launch) {
        final BackendData result = super.configureBackend(data, config, aMode,
                launch);
        result.setBeamLocator(new TestsBeamLocator(workdir));
        result.setTransient(true);
        return result;
    }

    @Override
    protected void postLaunch(final String amode, final IBackend backend,
            final IProgressMonitor monitor) throws CoreException {
        super.postLaunch(amode, backend, monitor);
        if (amode.equals("debug")) {
            initDebugger(monitor, backend);
        }
        startMonitorJob(monitor, backend);
        monitor.worked(1);
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
            // there is no make_links, ignore
        }
    }

    private void initDebugger(final IProgressMonitor monitor,
            final IBackend backend) {

        // TODO do we do this like this?
        // TODO how to remove old handlers? or at least disable them?

        final ErlangEventHandler handler = new ErlangEventHandler(
                "bterl_debugger", backend) {
            @Override
            public void handleEvent(final Event event) {
                System.out.println("BTERL DEBUG INIT");
                final String[] modules = workdir.list(new FilenameFilter() {
                    @Override
                    public boolean accept(final File dir, final String filename) {
                        return filename.endsWith(".erl");
                    }
                });

                for (final String pm : modules) {
                    ErlLogger.debug("reinterpret:: " + pm);
                    backend.interpret(project, pm, true, true);
                }
                backend.installDeferredBreakpoints();

                final OtpErlangPid pid = (OtpErlangPid) event
                        .getProperty("DATA");
                backend.send(pid, new OtpErlangAtom("ok"));
            }
        };
        handler.register();
    }

    private void startMonitorJob(final IProgressMonitor monitor,
            final IBackend backend) {

        // TODO do this in a job

        final ErlangEventHandler handler = new ErlangEventHandler(
                "bterl_monitor", backend) {
            @Override
            public void handleEvent(final Event event) {
                // TODO check events and do something
            }
        };
        handler.register();
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

        final String[] bterlPath = getBterlPath();
        System.out.println("... internal path = " + bterlPath);

        final String runtimeName = BackendCore.getRuntimeInfoManager()
                .getRuntime(new RuntimeVersion("R14B"), "").getName();
        // TODO how do we keep this updated?

        final List<String> paths = Lists.newArrayList();
        final String workdirPath = getOSIndependentPath(workdir);
        paths.add(workdirPath);
        for (final String path : bterlPath) {
            paths.add(path);
        }

        wc.setAttribute(ErlLaunchAttributes.RUNTIME_NAME, runtimeName);
        wc.setAttribute(ErlLaunchAttributes.NODE_NAME,
                "bterl" + System.currentTimeMillis());
        wc.setAttribute(ErlLaunchAttributes.START_ME, true);
        wc.setAttribute(ErlLaunchAttributes.WORKING_DIR, workdirPath);
        wc.setAttribute(ErlLaunchAttributes.CONSOLE, true);
        wc.setAttribute(ErlLaunchAttributes.COOKIE, "shade");
        wc.setAttribute(ErlLaunchAttributes.USE_LONG_NAME, false);
        final String args = "-boot start_clean -sasl sasl_error_logger false -pa "
                + ListsUtils.packList(paths, " -pa ");
        wc.setAttribute(ErlLaunchAttributes.EXTRA_ARGS, args);

        wc.setAttribute(ErlLaunchAttributes.PROJECTS, project);
        final List<String> modules = Lists.newArrayList();
        for (final String m : workdir.list(new FilenameFilter() {
            @Override
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
        final OtpErlangObject cb = ILaunchManager.DEBUG_MODE.equals(mode) ? BTERL_WATCHER_INIT_DEBUGGER
                : UNDEFINED;
        final String s = e_flags.toString() + "," + cmd.toString() + ","
                + trace.toString() + "," + cb;
        System.out.println(">>>>  * bt_run:start(" + s + ")");

        wc.setAttribute(ErlLaunchAttributes.MODULE, "bterl_watcher");
        wc.setAttribute(ErlLaunchAttributes.FUNCTION, "start_bterl");
        try {
            final String args2 = ErlUtils.format("{~x,~s,~x,~x,~s}", e_flags,
                    cmd, trace, cb, workdirPath).toString();
            wc.setAttribute(ErlLaunchAttributes.ARGUMENTS, args2);
        } catch (final TermParserException e) {
            e.printStackTrace();
        } catch (final SignatureException e) {
            e.printStackTrace();
        }

        wc.setModes(Sets.newHashSet(ILaunchManager.DEBUG_MODE,
                ILaunchManager.RUN_MODE));

        return wc;
    }

    private static String getOSIndependentPath(final File dir) {
        return new Path(dir.getAbsolutePath()).toPortableString();
    }

    public static String[] getBterlPath() throws CoreException {
        return new String[] { "/vobs/gsn/tools/3pp/erlang_bt_tool/bt_tool",
                "/vobs/gsn/tools/3pp/erlang_bt_tool/bt_tool/error_handler" };
    }

    private static Path getPluginPath() throws IOException {
        final Bundle bundle = Activator.getDefault().getBundle();
        URL url = FileLocator.find(bundle, new Path(""), null);
        url = FileLocator.resolve(url);
        return new Path(url.getPath());
    }

    @SuppressWarnings("unused")
    private void doLaunchWithScript(final File theWorkdir,
            final String theSuite, final String theTestCase,
            final String theMode, final ILaunch launch,
            final IProgressMonitor monitor) {
        System.out.println("---@> launch " + theWorkdir.getAbsolutePath()
                + " -> " + theSuite + ":" + theTestCase + " (" + theMode + ")");
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

}
