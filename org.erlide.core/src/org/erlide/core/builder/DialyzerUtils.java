package org.erlide.core.builder;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
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
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.util.Util;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideDialyze;

public class DialyzerUtils {

	public static final String DIALYZE_WARNING_MARKER = ErlangPlugin.PLUGIN_ID
			+ ".dialyzewarningmarker";

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

	public static void addDialyzeWarningMarkersFromResultList(
			final IErlProject project, final Backend backend,
			final OtpErlangList result) {
		if (result == null) {
			return;
		}
		final IProject p = project.getProject();
		final IPath projectPath = p.getLocation();
		final int projectPathLength = projectPath.toPortableString().length();
		for (final OtpErlangObject i : result) {
			final OtpErlangTuple t = (OtpErlangTuple) i;
			final OtpErlangTuple fileLine = (OtpErlangTuple) t.elementAt(1);
			final String filename = Util.stringValue(fileLine.elementAt(0));
			final OtpErlangLong lineL = (OtpErlangLong) fileLine.elementAt(1);
			final String relFilename = filename.substring(projectPathLength);
			final IPath relPath = Path.fromPortableString(relFilename);
			final IResource file = p.findMember(relPath);
			int line = 1;
			try {
				line = lineL.intValue();
			} catch (final OtpErlangRangeException e) {
				ErlLogger.error(e);
			}
			String s = ErlideDialyze.formatWarning(backend, t);
			final int j = s.indexOf(": ");
			if (j != -1) {
				s = s.substring(j + 1);
			}
			addDialyzeWarningMarker(file, s, line, IMarker.SEVERITY_WARNING);
		}
	}

	public static void addDialyzeWarningMarker(final IResource file,
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
			monitor.subTask("Dialyzing " + p.getName());
			final IProject project = p.getProject();
			removeDialyzerWarningMarkers(project);
			try {
				final Backend backend = ErlangCore.getBackendManager()
						.getBuildBackend(project);
				final List<String> files = new ArrayList<String>();
				final List<String> includeDirs = new ArrayList<String>();
				collectFilesAndIncludeDirs(p, modules, project, files,
						includeDirs, fromSource);
				final OtpErlangObject result = ErlideDialyze.dialyze(backend,
						files, pltPath, includeDirs, fromSource);
				checkDialyzeError(result);
				addDialyzeWarningMarkersFromResultList(p, backend,
						(OtpErlangList) result);
			} catch (final Exception e) {
				throw new InvocationTargetException(e);
			}
			monitor.worked(1);
		}
	}

	public static void collectFilesAndIncludeDirs(final IErlProject ep,
			final Map<IErlProject, Set<IErlModule>> modules,
			final IProject project, final List<String> files,
			final List<String> includeDirs, final boolean fromSource)
			throws CoreException {
		if (fromSource) {
			for (final IErlModule m : modules.get(ep)) {
				IResource resource;
				resource = m.getResource();
				files.add(resource.getLocation().toPortableString());
			}
		} else {
			final IFolder f = project.getFolder(ep.getOutputLocation());
			final IResource[] members = f.members(false);
			for (final IResource i : members) {
				IPath p = i.getLocation();
				if (p.toFile().exists())
				files.add(p.toPortableString());
			}
		}
		for (final String i : ep.getProperties().getIncludeDirs()) {
			final IPath path = new Path(i);
			project.getFile(path);
			IPath p= project.getLocation().append(i);
			final String s = p.toPortableString();
			if (p.toFile().exists()) {
				includeDirs.add(s);
			}
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
			final String s = Util.stringValue(t.elementAt(1));
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
