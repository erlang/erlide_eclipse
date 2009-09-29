/**
 *
 */
package org.erlide.core.builder.internal;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.builder.IMarkerGenerator;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.jinterface.util.ErlUtils;
import org.erlide.jinterface.util.TypeConverter;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class MarkerGenerator implements IMarkerGenerator {

	public static final String PROBLEM_MARKER = ErlangPlugin.PLUGIN_ID
			+ ".problemmarker";

	public static final String TASK_MARKER = ErlangPlugin.PLUGIN_ID
			+ ".taskmarker";

	public void addMarker(final IResource file, final IResource compiledFile,
			final String errorDesc, final int lineNumber, final int severity,
			final String errorVar) {
		MarkerGenerator.addProblemMarker(file, compiledFile, errorDesc,
				lineNumber, severity);
	}

	public static IResource findResourceByName(final IContainer container,
			final String fileName) {
		try {
			for (final IResource r : container.members()) {
				if (comparePath(r.getName(), fileName)) {
					return r;
				}
				if (r instanceof IContainer) {
					final IResource res = findResourceByName((IContainer) r,
							fileName);
					if (res != null) {
						return res;
					}
				}
			}
		} catch (final CoreException e) {
			e.printStackTrace();
		}
		return null;
	}

	public static boolean comparePath(final String p1, final String p2) {
		final boolean WINDOWS = java.io.File.separatorChar == '\\';
		if (WINDOWS) {
			return p1.equalsIgnoreCase(p2);
		} else {
			return p1.equals(p2);
		}
	}

	public static IResource findResource(final IContainer container,
			final String fileName) {
		try {
			for (final IResource r : container.members()) {
				if (comparePath(r.getName(), fileName)) {
					return r;
				}
				if (r instanceof IContainer) {
					final IResource res = findResource((IContainer) r, fileName);
					if (res != null) {
						return res;
					}
				}
			}
		} catch (final CoreException e) {
			e.printStackTrace();
		}
		return null;
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
	public static void addErrorMarkers(final IMarkerGenerator mg,
			final IResource resource, final OtpErlangList errorList) {
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
				if (!comparePath(resource.getLocation().toString(), fileName)) {
					res = findResource(resource.getProject(), fileName);
					if (res == null) {
						// TODO the error is in a file not in the project

						// try {
						// final String includeFile = ErlModelUtils
						// .findIncludeFile(project, res.getName(),
						// fExternalIncludes, pathVars);
						// if (includeFile != null) {
						// r = EditorUtility.openExternal(includeFile);
						// }
						// } catch (final Exception e) {
						// ErlLogger.warn(e);
						// }

						res = resource;
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

				mg.addMarker(res, resource, msg, line, sev, "");
			} catch (Exception e) {
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

	private static IMarker[] getMarkersFor(final IResource resource, String type) {
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

	private static void removeMarkerFor(final IResource resource, String type) {
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

}
