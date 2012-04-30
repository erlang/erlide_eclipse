package org.erlide.core.services.builder;

import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.erlide.backend.BackendCore;
import org.erlide.backend.IBackend;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.ModuleKind;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlElementLocator;
import org.erlide.core.model.root.IErlFolder;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.services.search.ErlideSearchServer;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.IRpcResultCallback;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.utils.SystemUtils;
import org.erlide.utils.Util;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

public class DialyzerUtils {

    private static final int MAX_MSG_LEN = 2000;
    private static BuilderHelper helper;

    public static void setHelper(final BuilderHelper h) {
        helper = h;
    }

    private static final class DialyzerCallback implements IRpcResultCallback {
        IBackend backend;
        private final SubMonitor monitor;
        private final String projectName;
        private final Object locker;

        DialyzerCallback(final IBackend backend, final SubMonitor monitor,
                final String projectName, final Object locker) {
            this.backend = backend;
            this.monitor = monitor;
            this.projectName = projectName;
            this.locker = locker;
        }

        @Override
        public void stop(final OtpErlangObject msg) {
            monitor.done();
            synchronized (locker) {
                locker.notifyAll();
            }
        }

        @Override
        public void start(final OtpErlangObject msg) {
            int progressMax;
            try {
                final OtpErlangLong progressMaxL = (OtpErlangLong) msg;
                progressMax = progressMaxL.intValue();
            } catch (final Exception e) {
                progressMax = 10;
            }
            final SubMonitor child = monitor.newChild(progressMax);
            child.setTaskName("Dialyzing " + projectName);
        }

        @Override
        public void progress(final OtpErlangObject msg) {
            final OtpErlangTuple t = (OtpErlangTuple) msg;
            final OtpErlangPid dialyzerPid = (OtpErlangPid) t.elementAt(0);
            final OtpErlangAtom whatA = (OtpErlangAtom) t.elementAt(1);
            final String what = whatA.toString();
            // ErlLogger.debug("Dialyzer %s", what);
            final OtpErlangObject result = t.elementAt(2);
            if (what.equals("warnings")) {
                MarkerUtils.addDialyzerWarningMarkersFromResultList(backend,
                        (OtpErlangList) result);
            } else if (what.equals("mod_deps")) {
                int remaining = 100;
                if (result instanceof OtpErlangLong) {
                    final OtpErlangLong l = (OtpErlangLong) result;
                    try {
                        remaining = l.intValue() * 5;
                    } catch (final OtpErlangRangeException e) {
                    }
                }
                monitor.setWorkRemaining(remaining);
            } else if (what.equals("log")) {
                monitor.worked(1);
            } else if (what.equals("EXIT")) {
                try {
                    checkDialyzeError(result);
                } catch (final DialyzerErrorException e) {
                    e.printStackTrace();
                }
            }
            if (monitor.isCanceled()) {
                try {
                    ErlideSearchServer.cancelSearch(BackendCore
                            .getBackendManager().getIdeBackend(), dialyzerPid);
                } catch (final RpcException e) {
                }
            }
        }
    }

    public static class DialyzerErrorException extends Exception {

        public DialyzerErrorException(final String message) {
            super(message);
        }

        /**
         *
         */
        private static final long serialVersionUID = -6872359945128662063L;

    }

    public static void doDialyze(final IProgressMonitor monitor,
            final Map<IErlProject, Set<IErlModule>> modules)
            throws InvocationTargetException {
        final Object locker = new Object();
        final Set<IErlProject> keySet = modules.keySet();
        for (final IErlProject p : keySet) {
            final IProject project = p.getWorkspaceProject();
            try {
                final DialyzerPreferences prefs = DialyzerPreferences
                        .get(project);
                final Collection<String> pltPaths = prefs.getEnabledPltPaths();
                final boolean fromSource = prefs.getFromSource();
                final boolean noCheckPLT = prefs.getNoCheckPLT();
                MarkerUtils.removeDialyzerMarkers(project);
                final IBackend backend = BackendCore.getBackendManager()
                        .getBuildBackend(project);
                final List<String> files = Lists.newArrayList();
                final List<IPath> includeDirs = Lists.newArrayList();
                final List<String> names = Lists.newArrayList();
                collectFilesAndIncludeDirs(p, modules, project, files, names,
                        includeDirs, fromSource);
                monitor.subTask("Dialyzing " + getFileNames(names));
                final IRpcResultCallback callback = new DialyzerCallback(
                        backend, SubMonitor.convert(monitor),
                        project.getName(), locker);
                try {
                    ErlideDialyze.startDialyzer(backend, files, pltPaths,
                            includeDirs, fromSource, noCheckPLT, callback);
                } catch (final RpcException e) {
                    throw new InvocationTargetException(e);
                }
                synchronized (locker) {
                    try {
                        locker.wait();
                    } catch (final InterruptedException e) {
                    }
                }

            } catch (final Exception e) {
                throw new InvocationTargetException(e);
            }
            monitor.worked(1);
        }
    }

    private static String getFileNames(final List<String> names) {
        if (names.size() == 0) {
            return "";
        }
        final StringBuilder sb = new StringBuilder(100);
        for (final String name : names) {
            if (sb.length() > 100) {
                sb.append("..., ");
                break;
            }
            sb.append(name);
            sb.append(", ");
        }
        return sb.substring(0, sb.length() - 2);
    }

    public static void collectFilesAndIncludeDirs(final IErlProject ep,
            final Map<IErlProject, Set<IErlModule>> modules,
            final IProject project, final Collection<String> files,
            final Collection<String> names,
            final Collection<IPath> includeDirs, final boolean fromSource)
            throws CoreException {
        final IFolder ebin = project.getFolder(ep.getOutputLocation());
        for (final IErlModule m : modules.get(ep)) {
            final String name = m.getName();
            if (ModuleKind.hasErlExtension(name)) {
                if (fromSource) {
                    final IResource resource = m.getResource();
                    files.add(resource.getLocation().toPortableString());
                } else {
                    final String moduleName = SystemUtils
                            .withoutExtension(name);
                    final String beamName = moduleName + ".beam";
                    final IResource beam = ebin.findMember(beamName);
                    if (beam != null) {
                        files.add(beam.getLocation().toPortableString());
                        names.add(moduleName);
                    }
                }
            }
        }
        if (helper != null) {
            helper.getIncludeDirs(project, includeDirs);
        }
    }

    public static void checkDialyzeError(final OtpErlangObject result)
            throws DialyzerErrorException {
        if (result == null) {
            throw new DialyzerErrorException(
                    "Could not execute dialyzer, please check settings.");
        }
        if (result instanceof OtpErlangTuple) {
            final OtpErlangTuple t = (OtpErlangTuple) result;
            if (t.arity() > 0) {
                final OtpErlangObject element = t.elementAt(0);
                if (element instanceof OtpErlangLong) {
                    final OtpErlangLong l = (OtpErlangLong) element;
                    try {
                        final int d = l.intValue();
                        if (d == 0 || d == 1 || d == 2) {
                            return;
                        }
                    } catch (final OtpErlangRangeException e) {
                    }
                }
            }
            final String s = Util.ioListToString(t.elementAt(1),
                    MAX_MSG_LEN + 10);
            final String r = s.replaceAll("\\\\n", "\n");
            if (s.length() > MAX_MSG_LEN) {
                ErlLogger.error("%s", s);
            }
            throw new DialyzerErrorException(r);
        }
    }

    public static void addModulesFromResource(final IErlElementLocator model,
            final IResource resource,
            final Map<IErlProject, Set<IErlModule>> modules)
            throws ErlModelException {
        final IErlElement element = model.findElement(resource, true);
        if (element == null) {
            return;
        }
        final IErlProject project = element.getProject();
        Set<IErlModule> ms = modules.get(project);
        if (ms == null) {
            ms = Sets.newHashSet();
        }
        if (element instanceof IErlFolder) {
            final IErlFolder folder = (IErlFolder) element;
            folder.open(null);
            ms.addAll(folder.getModules());
        } else if (element instanceof IErlModule) {
            final IErlModule module = (IErlModule) element;
            ms.add(module);
        } else if (element instanceof IErlProject) {
            project.open(null);
            ms.addAll(project.getModules());
        }
        modules.put(project, ms);
    }

}
