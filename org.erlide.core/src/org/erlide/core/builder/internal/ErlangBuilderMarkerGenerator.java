/**
 *
 */
package org.erlide.core.builder.internal;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.erlide.core.builder.ErlangBuilder;
import org.erlide.core.builder.IMarkerGenerator;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.jinterface.util.ErlUtils;
import org.erlide.jinterface.util.TypeConverter;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlangBuilderMarkerGenerator implements IMarkerGenerator {

	public void addMarker(final IResource file, final IResource compiledFile,
			final String errorDesc, final int lineNumber, final int severity,
			final String errorVar) {
		ErlangBuilderMarkerGenerator.addProblemMarker(file, compiledFile,
				errorDesc, lineNumber, severity);
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
			final IMarker marker = file.createMarker(ErlangBuilder.TASK_MARKER);
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
			final IMarker marker = file
					.createMarker(ErlangBuilder.PROBLEM_MARKER);
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
}
