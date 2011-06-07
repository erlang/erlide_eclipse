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
import org.erlide.core.backend.BackendCore;
import org.erlide.core.common.CommonUtils;
import org.erlide.core.common.Util;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.ModuleKind;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlFolder;
import org.erlide.core.model.root.IErlModel;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.rpc.IRpcCallSite;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

public class DialyzerUtils {

    private static BuilderHelper helper;

    public static void setHelper(final BuilderHelper h) {
        helper = h;
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
                final IRpcCallSite backend = BackendCore.getBackendManager()
                        .getBuildBackend(project);
                final List<String> files = Lists.newArrayList();
                final List<IPath> includeDirs = Lists.newArrayList();
                final List<String> names = Lists.newArrayList();
                collectFilesAndIncludeDirs(p, modules, project, files, names,
                        includeDirs, fromSource);
                monitor.subTask("Dialyzing " + getFileNames(names));
                OtpErlangObject result = ErlideDialyze.dialyze(backend, files,
                        pltPaths, includeDirs, fromSource, noCheckPLT);
                checkDialyzeError(result);
                if (result instanceof OtpErlangTuple) {
                    final OtpErlangTuple t = (OtpErlangTuple) result;
                    result = t.elementAt(1);
                }
                MarkerUtils.addDialyzerWarningMarkersFromResultList(p, backend,
                        (OtpErlangList) result);
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
                    final String beamName = CommonUtils.withoutExtension(name)
                            + ".beam";
                    final IResource beam = ebin.findMember(beamName);
                    if (beam != null) {
                        files.add(beam.getLocation().toPortableString());
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
            final String s = Util.ioListToString(t.elementAt(1), 2000)
                    .replaceAll("\\\\n", "\n");
            throw new DialyzerErrorException(s);
        }
    }

    public static void addModulesFromResource(final IErlModel model,
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
