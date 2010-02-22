package org.erlide.core.builder;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.core.ErlangPlugin;
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

	public static void removeDialyzerWarningMarkers(final IErlProject project) {
		try {
			final IMarker[] markers = project.getProject().findMarkers(
					DIALYZE_WARNING_MARKER, true, IResource.DEPTH_INFINITE);
			for (final IMarker m : markers) {
				m.delete();
			}
		} catch (final CoreException e) {
			ErlLogger.error(e);
		}

	}

	public static void addDialyzeWarningMarkersFromResult(
			final IErlProject project, final Backend backend,
			final OtpErlangList result) {
		final IProject p = project.getProject();
		final IPath projectPath = p.getLocation();
		final int projectPathLength = projectPath.toPortableString().length();
		for (final OtpErlangObject i : result) {
			final OtpErlangTuple t = (OtpErlangTuple) i;
			// final OtpErlangAtom warning = (OtpErlangAtom) t.elementAt(0);
			final OtpErlangTuple fileLine = (OtpErlangTuple) t.elementAt(1);
			final String filename = Util.stringValue(fileLine.elementAt(0));
			final OtpErlangLong lineL = (OtpErlangLong) fileLine.elementAt(1);
			final String relFilename = filename.substring(projectPathLength);
			final IPath relPath = Path.fromPortableString(relFilename);
			final IResource file = p.findMember(relPath);
			try {
				final int line = lineL.intValue();
				String s = ErlideDialyze.formatWarning(backend, t);
				final int j = s.indexOf(": ");
				if (j != -1) {
					s = s.substring(j + 1);
				}
				addDialyzeWarningMarker(file, s, line, IMarker.SEVERITY_WARNING);
			} catch (final OtpErlangRangeException e) {
				ErlLogger.error(e); // TODO show message
			}
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

}
