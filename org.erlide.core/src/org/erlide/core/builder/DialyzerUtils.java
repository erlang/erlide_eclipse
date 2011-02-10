package org.erlide.core.builder;

import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFolder;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.util.PreferencesUtils;
import org.erlide.jinterface.backend.util.Util;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

import erlang.ErlideDialyze;

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
            final Map<IErlProject, Set<IErlModule>> modules,
            final DialyzerPreferences prefs) throws InvocationTargetException {
        final boolean fromSource = prefs.getFromSource();
        final Set<IErlProject> keySet = modules.keySet();
        final List<String> pltPaths = PreferencesUtils.unpackList(prefs
                .getPltPath());
        for (final IErlProject p : keySet) {
            final IProject project = p.getProject();
            MarkerUtils.removeDialyzerMarkers(project);
            try {
                final Backend backend = ErlangCore.getBackendManager()
                        .getBuildBackend(project);
                final List<String> files = Lists.newArrayList();
                final List<IPath> includeDirs = Lists.newArrayList();
                final List<String> names = Lists.newArrayList();
                collectFilesAndIncludeDirs(p, modules, project, files, names,
                        includeDirs, fromSource);
                monitor.subTask("Dialyzing " + getFileNames(names));
                final OtpErlangObject result = ErlideDialyze.dialyze(backend,
                        files, pltPaths, includeDirs, fromSource);
                checkDialyzeError(result);
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
            if (ErlideUtil.hasErlExtension(name)) {
                if (fromSource) {
                    final IResource resource = m.getResource();
                    files.add(resource.getLocation().toPortableString());
                } else {
                    final String beamName = ErlideUtil.withoutExtension(name)
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
            final String s = Util.stringValue(t.elementAt(1)).replaceAll(
                    "\\\\n", "\n");
            throw new DialyzerErrorException(s);
        }
    }

    public static void addModulesFromResource(final IErlModel model,
            final IResource resource,
            final Map<IErlProject, Set<IErlModule>> modules)
            throws ErlModelException {
        final IErlElement e = model.findElement(resource, true);
        if (e instanceof IErlFolder) {
            final IErlFolder f = (IErlFolder) e;
            f.open(null);
            final Collection<IErlModule> folderModules = f.getModules();
            if (!folderModules.isEmpty()) {
                final IErlProject p = f.getErlProject();
                Set<IErlModule> ms = modules.get(p);
                if (ms == null) {
                    ms = new HashSet<IErlModule>();
                }
                ms.addAll(folderModules);
                modules.put(p, ms);
            }
        } else if (e instanceof IErlModule) {
            final IErlModule m = (IErlModule) e;
            final IErlProject p = m.getErlProject();
            Set<IErlModule> ms = modules.get(p);
            if (ms == null) {
                ms = new HashSet<IErlModule>();
            }
            ms.add(m);
            modules.put(p, ms);
        }
    }

}
