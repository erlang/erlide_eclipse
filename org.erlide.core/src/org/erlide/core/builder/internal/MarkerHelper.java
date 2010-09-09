/**
 *
 */
package org.erlide.core.builder.internal;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.builder.BuilderHelper;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlComment;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.core.erlang.util.ModelUtils;
import org.erlide.core.erlang.util.ResourceUtil;
import org.erlide.core.util.Tuple;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.jinterface.util.ErlUtils;
import org.erlide.jinterface.util.TypeConverter;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public final class MarkerHelper {

    private MarkerHelper() {
    }

    public static final String PROBLEM_MARKER = ErlangPlugin.PLUGIN_ID
            + ".problemmarker";

    public static final String TASK_MARKER = ErlangPlugin.PLUGIN_ID
            + ".taskmarker";

    public static void addMarker(final IResource file,
            final IResource compiledFile, final String errorDesc,
            final int lineNumber, final int severity, final String errorVar) {
        addProblemMarker(file, compiledFile, errorDesc, lineNumber, severity);
    }

    public static void addTaskMarker(final IResource file,
            final IResource compiledFile, final String message, int lineNumber,
            final int priority) {
        try {
            final IMarker marker = file.createMarker(TASK_MARKER);
            marker.setAttribute(IMarker.MESSAGE, message);
            marker.setAttribute(IMarker.PRIORITY, priority);
            marker.setAttribute(IMarker.SOURCE_ID, compiledFile.getFullPath()
                    .toString());
            if (lineNumber == -1) {
                lineNumber = 1;
            }
            marker.setAttribute(IMarker.LINE_NUMBER, lineNumber);
        } catch (final CoreException e) {
        }
    }

    /**
     * Add error markers from a list of error tuples
     *
     * @param resource
     * @param errorList
     */
    public static void addErrorMarkers(final IResource resource,
            final OtpErlangList errorList) {
        for (final OtpErlangObject odata : errorList.elements()) {
            try {
                final OtpErlangTuple data = (OtpErlangTuple) odata;

                String msg = ErlUtils.asString(data.elementAt(2));
                if (msg.length() > 1000) {
                    msg = msg.substring(0, 1000) + "......";
                }
                final String fileName = (String) TypeConverter.erlang2java(data
                        .elementAt(1), String.class);
                IResource res = resource;
                if (!BuilderHelper.samePath(resource.getLocation().toString(),
                        fileName)) {
                    final IProject project = resource.getProject();
                    res = BuilderHelper
                            .findResourceByLocation(project, fileName);
                    if (res == null) {
                        try {
                            final IErlModel model = ErlangCore.getModel();
                            final String includeFile = ModelUtils
                                    .findIncludeFile(
                                            project,
                                            fileName,
                                            model
                                                    .getExternal(
                                                            model
                                                                    .findProject(project),
                                                            ErlangCore.EXTERNAL_INCLUDES));
                            if (includeFile != null) {
                                res = ResourceUtil.openExternal(includeFile);
                            }
                        } catch (final Exception e) {
                            ErlLogger.warn(e);
                        }

                    }
                }
                int line = 0;
                if (data.elementAt(0) instanceof OtpErlangLong) {
                    try {
                        line = ((OtpErlangLong) data.elementAt(0)).intValue();
                    } catch (final OtpErlangRangeException e) {
                    }
                }
                int sev = IMarker.SEVERITY_INFO;
                try {
                    switch (((OtpErlangLong) data.elementAt(3)).intValue()) {
                    case 0:
                        sev = IMarker.SEVERITY_ERROR;
                        break;
                    case 1:
                        sev = IMarker.SEVERITY_WARNING;
                        break;
                    default:
                        sev = IMarker.SEVERITY_INFO;
                        break;
                    }
                } catch (final OtpErlangRangeException e) {
                }

                if (res != null) {
                    addMarker(res, resource, msg, line, sev, "");
                } else {
                    addMarker(resource.getProject(), null, "can't find "
                            + fileName, 0, IMarker.SEVERITY_ERROR, "");
                    addMarker(resource, null, "?? " + msg, line, sev, "");
                }
            } catch (final Exception e) {
                ErlLogger.warn(e);
                ErlLogger.warn("got: %s", odata);
            }
        }
    }

    public static void addProblemMarker(final IResource file,
            final IResource compiledFile, final String message, int lineNumber,
            final int severity) {
        try {
            final IMarker marker = file.createMarker(PROBLEM_MARKER);
            marker.setAttribute(IMarker.MESSAGE, message);
            marker.setAttribute(IMarker.SEVERITY, severity);
            if (compiledFile != null) {
                marker.setAttribute(IMarker.SOURCE_ID, compiledFile
                        .getFullPath().toString());
            }
            if (lineNumber == -1) {
                lineNumber = 1;
            }
            marker.setAttribute(IMarker.LINE_NUMBER, lineNumber);
        } catch (final CoreException e) {
        }
    }

    public static IMarker[] getProblemsFor(final IResource resource) {
        return getMarkersFor(resource, PROBLEM_MARKER);
    }

    public static IMarker[] getTasksFor(final IResource resource) {
        return getMarkersFor(resource, TASK_MARKER);
    }

    private static IMarker[] getMarkersFor(final IResource resource,
            final String type) {
        try {
            if (resource != null && resource.exists()) {
                return resource.findMarkers(type, false,
                        IResource.DEPTH_INFINITE);
            }
        } catch (final CoreException e) {
            // assume there are no tasks
        }
        return new IMarker[0];
    }

    public static void removeProblemsFor(final IResource resource) {
        removeMarkerFor(resource, PROBLEM_MARKER);
    }

    public static void removeTasksFor(final IResource resource) {
        removeMarkerFor(resource, TASK_MARKER);
    }

    private static void removeMarkerFor(final IResource resource,
            final String type) {
        try {
            if (resource != null && resource.exists()) {
                resource.deleteMarkers(type, false, IResource.DEPTH_INFINITE);
            }
        } catch (final CoreException e) {
            // assume there were no problems
        }
    }

    public static void removeProblemsAndTasksFor(final IResource resource) {
        try {
            if (resource != null && resource.exists()) {
                resource.deleteMarkers(PROBLEM_MARKER, false,
                        IResource.DEPTH_INFINITE);
                resource.deleteMarkers(TASK_MARKER, false,
                        IResource.DEPTH_INFINITE);
            }
        } catch (final CoreException e) {
            // assume there were no problems
        }
    }

    public static void deleteMarkers(final IResource resource) {
        try {
            resource.deleteMarkers(PROBLEM_MARKER, false, IResource.DEPTH_ZERO);
            resource.deleteMarkers(TASK_MARKER, false, IResource.DEPTH_ZERO);
            if (resource instanceof IFile) {
                deleteMarkersWithCompiledFile(resource.getProject(),
                        (IFile) resource);
                // should we delete markers for dependent hrl files?
            }
        } catch (final CoreException ce) {
        }
    }

    private static void deleteMarkersWithCompiledFile(final IProject project,
            final IFile file) {
        if (!project.isAccessible()) {
            return;
        }
        try {
            for (final IMarker m : project.findMarkers(PROBLEM_MARKER, true,
                    IResource.DEPTH_INFINITE)) {
                final Object source_id = m.getAttribute(IMarker.SOURCE_ID);
                if (source_id != null && source_id instanceof String
                        && source_id.equals(file.getFullPath().toString())) {
                    try {
                        m.delete();
                    } catch (final CoreException e) {
                        // not much to do
                    }
                }
            }
            for (final IMarker m : project.findMarkers(TASK_MARKER, true,
                    IResource.DEPTH_INFINITE)) {
                final Object source_id = m.getAttribute(IMarker.SOURCE_ID);
                if (source_id != null && source_id instanceof String
                        && source_id.equals(file.getFullPath().toString())) {
                    try {
                        m.delete();
                    } catch (final CoreException e) {
                        // not much to do
                    }
                }
            }
        } catch (final CoreException e) {
            // not much to do
        }
    }

    public void createProblemFor(final IResource resource,
            final IErlFunction erlElement, final String message,
            final int problemSeverity) throws CoreException {
        try {
            final IMarker marker = resource.createMarker(PROBLEM_MARKER);
            final int severity = problemSeverity;

            final ISourceRange range = erlElement == null ? null : erlElement
                    .getNameRange();
            final int start = range == null ? 0 : range.getOffset();
            final int end = range == null ? 1 : start + range.getLength();
            marker.setAttributes(new String[] { IMarker.MESSAGE,
                    IMarker.SEVERITY, IMarker.CHAR_START, IMarker.CHAR_END },
                    new Object[] { message, Integer.valueOf(severity),
                            Integer.valueOf(start), Integer.valueOf(end) });
        } catch (final CoreException e) {
            throw e;
        }
    }

    public static void createTaskMarkers(final IProject project,
            final IResource resource) {
        final IErlProject p = ErlangCore.getModel().findProject(project);
        if (p != null) {
            try {
                if (BuilderHelper.isDebugging()) {
                    ErlLogger.debug("Creating task markers "
                            + resource.getName());
                }
                // getMarkersFor(resource, p);
                getNoScanMarkersFor(resource, p);
            } catch (final ErlModelException e) {
            }
        }

    }

    @SuppressWarnings("unused")
    private static void getMarkersFor(final IResource resource,
            final IErlProject p) throws ErlModelException {
        final IErlModule m = p.getModule(resource.getName());
        if (m == null) {
            return;
        }
        m.getScanner();
        final Collection<IErlComment> cl = m.getComments();
        for (final IErlComment c : cl) {
            final String text = c.getName();
            final int line = c.getLineStart();
            mkMarker(resource, line, text, "TODO", IMarker.PRIORITY_NORMAL);
            mkMarker(resource, line, text, "XXX", IMarker.PRIORITY_NORMAL);
            mkMarker(resource, line, text, "FIXME", IMarker.PRIORITY_HIGH);
        }
        m.disposeScanner();
    }

    private static void getNoScanMarkersFor(final IResource resource,
            final IErlProject p) throws ErlModelException {
        if (!(resource instanceof IFile)) {
            return;
        }
        final IFile file = (IFile) resource;
        InputStream input;
        try {
            input = file.getContents();
            final BufferedReader reader = new BufferedReader(
                    new InputStreamReader(input));
            try {
                String line = reader.readLine();
                final List<Tuple<String, Integer>> cl = new ArrayList<Tuple<String, Integer>>();
                int numline = 0;
                while (line != null) {
                    if (line.matches("^[^%]*%+[ \t]*(TODO|XXX|FIXME).*")) {
                        cl.add(new Tuple<String, Integer>(line, numline));
                    }
                    numline++;
                    line = reader.readLine();
                }

                for (final Tuple<String, Integer> c : cl) {
                    mkMarker(resource, c.o2, c.o1, "TODO",
                            IMarker.PRIORITY_NORMAL);
                    mkMarker(resource, c.o2, c.o1, "XXX",
                            IMarker.PRIORITY_NORMAL);
                    mkMarker(resource, c.o2, c.o1, "FIXME",
                            IMarker.PRIORITY_HIGH);
                }
            } finally {
                reader.close();
            }
        } catch (final CoreException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (final IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    private static void mkMarker(final IResource resource, final int line,
            final String text, final String tag, final int prio) {
        if (text.contains(tag)) {
            final int ix = text.indexOf(tag);
            final String msg = text.substring(ix);
            int dl = 0;
            for (int i = 0; i < ix; i++) {
                if (text.charAt(i) == '\n') {
                    dl++;
                }
            }
            addTaskMarker(resource, resource, msg, line + 1 + dl, prio);
        }
    }

}
