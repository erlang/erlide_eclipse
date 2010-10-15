package org.erlide.core.builder;

import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFolder;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.core.erlang.util.ResourceUtil;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.util.Util;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

import erlang.ErlideDialyze;

public class DialyzerUtils {

    public static final String DIALYZE_WARNING_MARKER = ErlangPlugin.PLUGIN_ID
            + ".dialyzewarningmarker";

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

    public static void removeDialyzerWarningMarkers(final IProject project) {
        try {
            final IMarker[] markers = project.findMarkers(
                    DIALYZE_WARNING_MARKER, true, IResource.DEPTH_INFINITE);
            for (final IMarker m : markers) {
                m.delete();
            }
        } catch (final CoreException e) {
            ErlLogger.error(e);
        }

    }

    public static void addDialyzerWarningMarkersFromResultList(
            final IErlProject project, final Backend backend,
            final OtpErlangList result) {
        if (result == null) {
            return;
        }
        final IProject p = project.getProject();
        for (final OtpErlangObject i : result) {
            final OtpErlangTuple t = (OtpErlangTuple) i;
            final OtpErlangTuple fileLine = (OtpErlangTuple) t.elementAt(1);
            final String filename = Util.stringValue(fileLine.elementAt(0));
            final OtpErlangLong lineL = (OtpErlangLong) fileLine.elementAt(1);
            int line = 1;
            try {
                line = lineL.intValue();
            } catch (final OtpErlangRangeException e) {
                ErlLogger.error(e);
            }
            String s = ErlideDialyze.formatWarning(backend, t).trim();
            final int j = s.indexOf(": ");
            if (j != -1) {
                s = s.substring(j + 1);
            }
            addDialyzerWarningMarker(p, filename, line, s);
        }
    }

    public static void addDialyzerWarningMarker(final IProject p,
            final String filename, final int line, final String message) {
        final IPath projectPath = p.getLocation();
        final String projectPathString = projectPath.toPortableString();
        IResource file;
        if (filename.startsWith(projectPathString)) {
            final String relFilename = filename.substring(projectPathString
                    .length());
            final IPath relPath = Path.fromPortableString(relFilename);
            file = p.findMember(relPath);
        } else {
            try {
                file = ResourceUtil.openExternal(filename);
            } catch (final CoreException e) {
                ErlLogger.error(e);
                file = p;
            }
        }
        addDialyzerWarningMarker(file, message, line, IMarker.SEVERITY_WARNING);
    }

    public static void addDialyzerWarningMarker(final IResource file,
            final String message, int lineNumber, final int severity) {
        try {
            final IMarker marker = file.createMarker(DIALYZE_WARNING_MARKER);
            marker.setAttribute(IMarker.MESSAGE, message);
            marker.setAttribute(IMarker.SEVERITY, severity);
            if (lineNumber == -1) {
                lineNumber = 1;
            }
            marker.setAttribute(IMarker.LINE_NUMBER, lineNumber);
        } catch (final CoreException e) {
        }
    }

    public static void doDialyze(final IProgressMonitor monitor,
            final Map<IErlProject, Set<IErlModule>> modules,
            final DialyzerPreferences prefs) throws InvocationTargetException {
        final boolean fromSource = prefs.getFromSource();
        final Set<IErlProject> keySet = modules.keySet();
        final String pltPath = prefs.getPltPath();
        for (final IErlProject p : keySet) {
            final IProject project = p.getProject();
            removeDialyzerWarningMarkers(project);
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
                        files, pltPath, includeDirs, fromSource);
                checkDialyzeError(result);
                addDialyzerWarningMarkersFromResultList(p, backend,
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
            final IErlProject p = f.getErlProject();
            Set<IErlModule> ms = modules.get(p);
            if (ms == null) {
                ms = new HashSet<IErlModule>();
            }
            ms.addAll(f.getModules());
            modules.put(p, ms);
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
