package org.erlide.dialyzer.builder;

import java.util.List;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.builder.MarkerUtils;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.SourceKind;
import org.erlide.engine.model.root.IErlElementLocator;
import org.erlide.engine.util.ResourceUtil;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.util.ErlLogger;
import org.erlide.util.Util;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class DialyzerMarkerUtils {

    public static final String PATH_ATTRIBUTE = "org.eclipse.ui.views.markers.path";//$NON-NLS-1$
    public static final String DIALYZE_WARNING_MARKER = "org.erlide.dialyzer.core"
            + ".dialyzewarningmarker";
    public static final String PROBLEM_MARKER = "org.erlide.dialyzer.core"
            + ".problemmarker";

    public static void addDialyzerWarningMarkersFromResultList(final IOtpRpc backend,
            final OtpErlangList result) {
        if (result == null) {
            return;
        }
        final List<String> warnings = ErlideDialyze.formatWarnings(backend, result);
        for (int i = 0; i < warnings.size(); i++) {
            final OtpErlangTuple t = (OtpErlangTuple) result.elementAt(i);
            final OtpErlangTuple fileLine = (OtpErlangTuple) t.elementAt(1);
            final String filename = Util.stringValue(fileLine.elementAt(0));
            final OtpErlangLong lineL = (OtpErlangLong) fileLine.elementAt(1);
            if (!filename.isEmpty()) {
                int line = 1;
                try {
                    line = lineL.intValue();
                } catch (final OtpErlangRangeException e) {
                    ErlLogger.error(e);
                }
                if (line <= 0) {
                    line = 1;
                }

                String msg = warnings.get(i);
                final int j = msg.indexOf(": ");
                if (j != -1) {
                    msg = msg.substring(j + 1);
                }
                final IErlElementLocator model = ErlangEngine.getInstance().getModel();
                addDialyzerWarningMarker(model, filename, line, msg);
            }
        }
    }

    public static void addDialyzerWarningMarker(final IErlElementLocator model,
            final String path, final int line, final String message) {
        IResource resource = null;
        IErlModule module = null;
        try {
            if (SourceKind.hasHrlExtension(path)) {
                module = model.findInclude(null, path);
            } else {
                module = model.findModule(null, path);
            }
            if (module != null) {
                resource = module.getResource();
            }
        } catch (final Exception e) {
            ErlLogger.error(e);
        }

        if (resource == null) {
            resource = ResourceUtil.getFileFromLocation(path);
        }
        if (resource != null) {
            addMarker(resource, path, message, line, IMarker.SEVERITY_WARNING,
                    DIALYZE_WARNING_MARKER);
        }
    }

    public static void addMarker(final IResource resource, final String path,
            final String message, final int lineNumber, final int severity,
            final String markerKind) {
        try {
            final IMarker marker = resource.createMarker(markerKind);
            marker.setAttribute(IMarker.MESSAGE, message);
            marker.setAttribute(IMarker.SEVERITY, severity);
            marker.setAttribute(IMarker.LINE_NUMBER, lineNumber);
            marker.setAttribute(PATH_ATTRIBUTE, path);
        } catch (final CoreException e) {
            ErlLogger.debug(e);
        }
    }

    public static void removeDialyzerMarkersFor(final IResource resource) {
        removeMarkersFor(resource, DIALYZE_WARNING_MARKER);
    }

    public static boolean haveDialyzerMarkers(final IResource resource) {
        try {
            if (resource.isAccessible()) {
                final IMarker[] markers = resource.findMarkers(DIALYZE_WARNING_MARKER,
                        true, IResource.DEPTH_INFINITE);
                return markers != null && markers.length > 0;
            }
        } catch (final CoreException e) {
            ErlLogger.error(e);
        }
        return false;
    }

    private static void removeMarkersFor(final IResource resource, final String type) {
        try {
            if (resource != null && resource.exists()) {
                resource.deleteMarkers(type, false, IResource.DEPTH_INFINITE);
            }
        } catch (final CoreException e) {
            // assume there were no problems
        }
    }

    public static void addMarker(final IResource file, final String path,
            final IResource compiledFile, final String errorDesc, final int lineNumber,
            final int severity, final String errorVar) {
        addProblemMarker(file, path, compiledFile, errorDesc, lineNumber, severity);
    }

    public static void addProblemMarker(final IResource resource, final String path,
            final IResource compiledFile, final String message, final int lineNumber,
            final int severity) {
        try {
            final IMarker marker = resource.createMarker(PROBLEM_MARKER);
            marker.setAttribute(IMarker.MESSAGE, message);
            marker.setAttribute(IMarker.SEVERITY, severity);
            if (path != null && !new Path(path).equals(resource.getLocation())) {
                marker.setAttribute(MarkerUtils.PATH_ATTRIBUTE, path);
            }
            if (compiledFile != null) {
                marker.setAttribute(IMarker.SOURCE_ID, compiledFile.getFullPath()
                        .toString());
            }
            marker.setAttribute(IMarker.LINE_NUMBER, lineNumber != -1 ? lineNumber : 1);
        } catch (final CoreException e) {
        }
    }

}
