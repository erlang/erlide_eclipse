package org.erlide.core.builder;

import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.erlide.backend.BackendException;
import org.erlide.backend.IBackend;
import org.erlide.model.ErlModelException;
import org.erlide.model.erlang.IErlModule;
import org.erlide.model.erlang.ModuleKind;
import org.erlide.model.root.IErlElement;
import org.erlide.model.root.IErlElementLocator;
import org.erlide.model.root.IErlFolder;
import org.erlide.model.root.IErlProject;
import org.erlide.model.util.ModelUtils;
import org.erlide.runtime.IRpcSite;
import org.erlide.runtime.rpc.IRpcFuture;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.runtime.rpc.RpcTimeoutException;
import org.erlide.util.ErlLogger;
import org.erlide.util.SystemConfiguration;
import org.erlide.util.Util;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
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

    public static class DialyzerErrorException extends Exception {

        public DialyzerErrorException(final String message) {
            super(message);
        }

        private static final long serialVersionUID = -6872359945128662063L;
    }

    public static void doDialyze(final IProgressMonitor monitor,
            final Set<IErlModule> modules, final Set<IErlProject> projects,
            final IBackend backend) throws InvocationTargetException,
            DialyzerErrorException {
        try {
            for (final IErlModule module : modules) {
                DialyzerMarkerUtils.removeDialyzerMarkersFor(module
                        .getResource());
            }

            // TODO handle preferences from multiple projects
            final DialyzerPreferences prefs = DialyzerPreferences.get(null);
            final Collection<String> pltPaths = prefs.getPltPaths();
            final boolean fromSource = false; // prefs.getFromSource();
            final boolean noCheckPLT = true; // prefs.getNoCheckPLT();

            final List<String> files = Lists.newArrayList();
            final List<IPath> includeDirs = Lists.newArrayList();
            final List<String> names = Lists.newArrayList();
            collectFilesAndIncludeDirs(modules, projects, files, names,
                    includeDirs, fromSource);

            ErlLogger.debug("Dialyzing %s %s", names.size(),
                    Arrays.toString(names.toArray()));
            monitor.subTask("Dialyzing " + getFileNames(names));
            final IRpcSite b = backend.getRpcSite();
            final IRpcFuture future = ErlideDialyze.dialyze(b, files, pltPaths,
                    includeDirs, fromSource, noCheckPLT);

            while (!future.isDone()) {
                // check cancellation
                if (monitor.isCanceled()) {
                    throw new OperationCanceledException();
                }
                // check backend down
                if (backend.isStopped()) {
                    throw new BackendException("Backend " + backend.getName()
                            + " is down");
                }

                OtpErlangObject r = null;
                try {
                    r = future.get(500);
                } catch (final RpcTimeoutException e) {
                }
                if (r != null) {
                    processResult(b, r);
                }
            }
        } catch (final RpcException e) {
            throw new InvocationTargetException(e);
        } catch (final CoreException e) {
            throw new InvocationTargetException(e);
        } catch (final BackendException e) {
            throw new InvocationTargetException(e);
        }
    }

    private static void processResult(final IRpcSite backend,
            final OtpErlangObject o) throws DialyzerErrorException {
        if (o instanceof OtpErlangTuple) {
            final OtpErlangTuple t = (OtpErlangTuple) o;
            final OtpErlangAtom whatA = (OtpErlangAtom) t.elementAt(0);
            final String what = whatA.toString();
            final OtpErlangObject result = t.elementAt(1);

            if (what.equals("warnings")) {
                DialyzerMarkerUtils.addDialyzerWarningMarkersFromResultList(
                        backend, (OtpErlangList) result);
            } else if (what.equals("dialyzer_error")) {
                final String s = Util.ioListToString(result, MAX_MSG_LEN);
                throw new DialyzerErrorException(s);
            }
        } else {
            throw new DialyzerErrorException("Unknown Dialyzer message: "
                    + Util.ioListToString(o, MAX_MSG_LEN));
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

    public static void collectFilesAndIncludeDirs(
            final Set<IErlModule> modules, final Set<IErlProject> projects,
            final Collection<String> files, final Collection<String> names,
            final Collection<IPath> includeDirs, final boolean fromSource) {
        for (final IErlModule m : modules) {
            final String name = m.getName();
            final IErlProject erlProject = ModelUtils.getProject(m);
            final IProject project = erlProject.getWorkspaceProject();
            final IFolder ebin = project.getFolder(erlProject
                    .getOutputLocation());
            if (ModuleKind.hasErlExtension(name)) {
                if (fromSource) {
                    final IResource resource = m.getResource();
                    files.add(resource.getLocation().toPortableString());
                } else {
                    final String moduleName = SystemConfiguration
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
        helper = new BuilderHelper();
        for (final IErlProject p : projects) {
            helper.getIncludeDirs(p.getWorkspaceProject(), includeDirs);
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

    public static Set<IErlModule> collectModulesFromResource(
            final IErlElementLocator model, final IResource resource)
            throws ErlModelException {
        final Set<IErlModule> result = Sets.newHashSet();
        final IErlElement element = model.findElement(resource, true);
        if (element == null) {
            return result;
        }
        if (element instanceof IErlFolder) {
            final IErlFolder folder = (IErlFolder) element;
            folder.open(null);
            result.addAll(folder.getModules());
        } else if (element instanceof IErlModule) {
            final IErlModule module = (IErlModule) element;
            result.add(module);
        } else if (element instanceof IErlProject) {
            final IErlProject project = (IErlProject) element;
            project.open(null);
            result.addAll(project.getModules());
        }
        return result;
    }

}
