/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.builder;

import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.IOException;
import java.util.Date;
import java.util.Map;

import org.apache.commons.net.bsd.RLoginClient;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.erlide.basiccore.ErlLogger;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.IMarkerGenerator;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.core.util.RemoteConnector;
import org.erlide.runtime.ErlangProjectProperties;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.IBackendVisitor;
import org.erlide.runtime.backend.RpcResult;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * @author Vlad Dumitrescu
 */
public class ErlangBuilder extends IncrementalProjectBuilder implements
		IMarkerGenerator {

	protected static final String PROBLEM_MARKER = ErlangPlugin.PLUGIN_ID
			+ ".problemmarker";

	protected static final String TASK_MARKER = ErlangPlugin.PLUGIN_ID
			+ ".taskmarker";

	private static final String MODULE = "erlide_builder";

	// TODO how do we configure builder?
	// use the internal builder
	private final boolean fInternal = true;

	static class ErlangBuilderMarkerGenerator implements IMarkerGenerator {

		public void addMarker(IResource file, String errorDesc, int lineNumber,
				int severity, String errorVar) {
			ErlangBuilder.addMarker(file, errorDesc, lineNumber, severity);
		}
	};

	static private IMarkerGenerator getMarkerGenerator() {
		return new ErlangBuilder.ErlangBuilderMarkerGenerator();
	}

	private static void addMarker(IResource file, String message,
			int lineNumber, int severity) {
		try {
			final IMarker marker = file.createMarker(PROBLEM_MARKER);
			marker.setAttribute(IMarker.MESSAGE, message);
			marker.setAttribute(IMarker.SEVERITY, severity);
			if (lineNumber == -1) {
				lineNumber = 1;
			}
			marker.setAttribute(IMarker.LINE_NUMBER, lineNumber);
		} catch (final CoreException e) {
		}
	}

	protected void createProblemFor(IResource resource,
			IErlFunction erlElement, String message, int problemSeverity)
			throws CoreException {
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

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.internal.events.InternalBuilder#build(int,
	 *      java.util.Map, org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	protected IProject[] build(int kind, Map args, IProgressMonitor monitor)
			throws CoreException {
		// ReflectionUtils.exportClass(ErlangBuilder.class);
		// if (monitor == null)
		// {
		// ErlLogger.debug("** null monitor in build!?");
		// monitor = new NullProgressMonitor();
		// }

		if (isDebugging()) {
			ErlLogger.debug("building!...");
		}
		if (fInternal) {
			if (kind == FULL_BUILD) {
				fullBuild(args, monitor);
			} else {
				final IResourceDelta delta = getDelta(getProject());
				if (hasHrl(delta)) {
					fullBuild(args, monitor);
				} else {
					incrementalBuild(args, delta, monitor);
				}
			}
		} else {
			build_remote(kind, args, monitor);
		}

		checkForClashes();

		return null;
	}

	private void checkForClashes() {
		try {
			getProject().deleteMarkers(PROBLEM_MARKER, true,
					IResource.DEPTH_ZERO);
		} catch (CoreException e1) {
		}

		IBackend b = BackendManager.getDefault().getIdeBackend();
		try {
			OtpErlangList res = (OtpErlangList) b.rpcx("erlide_builder",
					"code_clash");
			for (int i = 0; i < res.arity(); i++) {
				OtpErlangTuple t = (OtpErlangTuple) res.elementAt(i);
				String f1 = ((OtpErlangString) t.elementAt(0)).stringValue();
				String f2 = ((OtpErlangString) t.elementAt(1)).stringValue();
				addMarker(getProject(), "Code clash between " + f1 + " and "
						+ f2, 0, IMarker.SEVERITY_WARNING, "");
			}

		} catch (Exception e) {
		}
		try {
			ErlangProjectProperties pp = new ErlangProjectProperties(
					getProject());
			String[] sd = pp.getSourceDirs();
			String[] dirList = new String[sd.length];
			for (int i = 0; i < sd.length; i++) {
				dirList[i] = getProject().getLocation().toPortableString()
						+ "/" + sd[i];
			}
			OtpErlangList res = (OtpErlangList) b.rpcx("erlide_builder",
					"source_clash", (Object) dirList);
			for (int i = 0; i < res.arity(); i++) {
				OtpErlangTuple t = (OtpErlangTuple) res.elementAt(i);
				String f1 = ((OtpErlangString) t.elementAt(0)).stringValue();
				String f2 = ((OtpErlangString) t.elementAt(1)).stringValue();
				addMarker(getProject(), "Duplicated module name in " + f1
						+ " and " + f2, 0, IMarker.SEVERITY_ERROR, "");
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private boolean hasHrl(IResourceDelta delta) {
		if (delta == null) {
			return false;
		}

		final HrlDeltaVisitor v = new HrlDeltaVisitor();
		try {
			delta.accept(v);
		} catch (final CoreException e) {
		}
		return v.hasAny;
	}

	protected void deleteMarkers(IFile file) {
		try {
			file.deleteMarkers(PROBLEM_MARKER, false, IResource.DEPTH_ZERO);
		} catch (final CoreException ce) {
		}
	}

	@SuppressWarnings("unchecked")
	protected void fullBuild(Map args, final IProgressMonitor monitor)
			throws CoreException {
		if (isDebugging()) {
			ErlLogger.debug("full build...");
		}
		monitor.beginTask("Performing full build", 100);
		try {
			// IWorkspaceRunnable op = new
			// CleanOutFoldersOperation(getProject());
			// ResourcesPlugin.getWorkspace().run(op, new
			// SubProgressMonitor(monitor,
			// 15));

			final IProgressMonitor subMon = new SubProgressMonitor(monitor, 100);
			subMon.beginTask("Compiling", IProgressMonitor.UNKNOWN);
			try {
				getProject().accept(new ErlangResourceVisitor(subMon));
			} finally {
				subMon.done();
			}
		} finally {
			monitor.done();
		}
	}

	@SuppressWarnings("unchecked")
	protected void incrementalBuild(Map args, IResourceDelta delta,
			IProgressMonitor monitor) throws CoreException {
		if (isDebugging()) {
			ErlLogger.debug("incr build...");
		}
		final IResourceDelta[] chd = delta.getAffectedChildren();
		final int n = chd.length * 100;
		monitor.beginTask("Compiling Erlang files", n);
		try {
			// the visitor does the work.
			delta.accept(new ErlangDeltaVisitor(monitor));
		} finally {
			monitor.done();
		}
	}

	/**
	 * Method clean
	 * 
	 * @param monitor
	 *            IProgressMonitor
	 * @throws CoreException
	 */
	@Override
	protected void clean(IProgressMonitor monitor) throws CoreException {
		if (isDebugging()) {
			ErlLogger.debug("cleaning...");
		}

		super.clean(monitor);
		getProject().deleteMarkers(PROBLEM_MARKER, true,
				IResource.DEPTH_INFINITE);

		// TODO also delete beam files

		final ErlangProjectProperties prefs = new ErlangProjectProperties(
				getProject());

		// delete beam files
		final IFolder bf = getProject().getFolder(prefs.getOutputDir());
		final IResource[] beams = bf.members();
		for (IResource element : beams) {
			if ("beam".equals(element.getFileExtension())) {
				element.delete(true, monitor);
			}
		}

	}

	/**
	 * Method compileFile
	 * 
	 * @param project
	 *            IProject
	 * @param resource
	 *            IResource
	 */
	protected void compileFile(IProject project, IResource resource) {
		final IPath projectPath = project.getLocation();
		final ErlangProjectProperties prefs = new ErlangProjectProperties(
				project);

		try {
			resource.deleteMarkers(PROBLEM_MARKER, true,
					IResource.DEPTH_INFINITE);
		} catch (final CoreException e1) {
		}
		if (isInExtCodePath(resource, project)
				&& !isInCodePath(resource, project)) {
			addMarker(
					resource,
					resource.getName()
							+ " is not directly on source path (packages are not supported yet)",
					0, IMarker.SEVERITY_WARNING, "");
			return;
		}

		final String outputDir = projectPath.append(prefs.getOutputDir())
				.toString();
		// TODO leave this in place??
		// if (BackendManager.isDeveloper()) {
		// IBackend b = BackendManager.getDefault().get(project);
		// outputDir += "/" + b.getCurrentVersion();
		// }
		ensureDirExists(outputDir);

		final String[] incs = prefs.getIncludeDirs();
		final String[] includeDirs = new String[incs.length];
		for (int i = 0; i < incs.length; i++) {
			final IPath inc = new Path(incs[i]);
			if (inc.isAbsolute()) {
				includeDirs[i] = inc.toString();
			} else {
				includeDirs[i] = projectPath.append(incs[i]).toString();
			}
		}

		// delete beam file
		IPath beam = new Path(prefs.getOutputDir());
		final IPath module = beam.append(resource.getName())
				.removeFileExtension();
		beam = module.addFileExtension("beam").setDevice(null);
		IResource br = project.findMember(beam);

		try {
			if (br != null) {
				br.delete(true, null);
			}

			OtpErlangObject r;

			r = compileFile(project, resource.getLocation().toString(),
					outputDir, includeDirs);
			if (r == null) {
				return;
			}
			final OtpErlangTuple t = (OtpErlangTuple) r;
			// ErlLogger.debug("** " + r);

			if ("ok".equals(((OtpErlangAtom) t.elementAt(0)).atomValue())) {
				final String beamf = resource.getFullPath()
						.removeFileExtension().lastSegment();
				// ErlLogger.debug(">>>>> " + project.getName() + ": " +
				// beamf);
				if (project.getName().startsWith("erlide")
						|| beamf.startsWith("erlide")) {
					// ErlLogger.debug(">>>>> is erlide");
					if (BackendManager.isDeveloper()) {
						// ErlLogger.debug(">>>>> is developer");
						final OtpErlangBinary code = (OtpErlangBinary) t
								.elementAt(2);
						distributeModule(beamf, code);
					}
				} else {
					// ErlLogger.debug(">>>>> normal");
					loadModule(project, beamf);
				}
			} else {
				ErlLogger.debug(">>>> compile error..." + resource.getName());
			}

			if (br != null) {
				br.getParent().refreshLocal(IResource.DEPTH_ONE, null);
			}
			br = project.findMember(new Path(prefs.getOutputDir()));
			if (br != null) {
				br.refreshLocal(IResource.DEPTH_ONE, null);
			}
			br = project.findMember(beam);
			if (br != null) {
				br.setDerived(true);
			}

			// process compilation messages
			final OtpErlangList l = (OtpErlangList) t.elementAt(1);
			addErrorMarkers(getMarkerGenerator(), resource, l);
		} catch (final Exception e) {
			e.printStackTrace();
		}

	}

	private void ensureDirExists(String outputDir) {
		final File f = new File(outputDir);
		f.mkdir();
	}

	protected void compileYrlFile(IProject project, IResource resource) {
		// final IPath projectPath = project.getLocation();
		// final ErlangProjectProperties prefs = new
		// ErlangProjectProperties(project);

		try {
			resource.deleteMarkers(PROBLEM_MARKER, true,
					IResource.DEPTH_INFINITE);
		} catch (final CoreException e1) {
		}
		if (isInExtCodePath(resource, project)
				&& !isInCodePath(resource, project)) {
			addMarker(
					resource,
					resource.getName()
							+ " is not directly on source path (packages are not supported yet)",
					0, IMarker.SEVERITY_WARNING, "");
			return;
		}

		IPath erl = resource.getProjectRelativePath().removeFileExtension();
		erl = erl.addFileExtension("erl").setDevice(null);
		IResource br = project.findMember(erl);

		try {
			if (br != null) {
				br.delete(true, null);
			}

			OtpErlangObject r;

			final String input = resource.getLocation().toString();
			final String output = resource.getLocation().removeFileExtension()
					.toString();
			r = compileYrlFile(project, input, output);

			if (r instanceof OtpErlangTuple) {
				// process compilation messages
				final OtpErlangTuple t = (OtpErlangTuple) r;
				final OtpErlangList l = (OtpErlangList) t.elementAt(1);
				addErrorMarkers(getMarkerGenerator(), resource, l);
			}

			resource.getParent().refreshLocal(IResource.DEPTH_ONE, null);
			br = project.findMember(erl);
			if (br != null) {
				br.setDerived(true);
				// br.touch() doesn't work...
				compileFile(project, br);
			}

		} catch (final Exception e) {
			e.printStackTrace();
		}

	}

	private static boolean isInCodePath(IResource resource, IProject project) {
		final ErlangProjectProperties prefs = new ErlangProjectProperties(
				project);
		final IPath projectPath = project.getLocation();

		final String[] srcs = prefs.getSourceDirs();
		for (String element : srcs) {
			final IPath sp = projectPath.append(new Path(element));
			if (sp.equals(resource.getLocation().removeLastSegments(1))) {
				return true;
			}
		}

		return false;
	}

	private static boolean isInExtCodePath(IResource resource, IProject project) {
		final ErlangProjectProperties prefs = new ErlangProjectProperties(
				project);
		final IPath projectPath = project.getLocation();

		final String[] srcs = prefs.getSourceDirs();
		for (String element : srcs) {
			final IPath sp = projectPath.append(new Path(element));
			if (sp.isPrefixOf(resource.getLocation())) {
				return true;
			}
		}

		return false;
	}

	private static boolean isInOutputPath(IResource resource, IProject project) {
		final ErlangProjectProperties prefs = new ErlangProjectProperties(
				project);
		final IPath projectPath = project.getLocation();

		final String out = prefs.getOutputDir();
		return projectPath.append(new Path(out)).isPrefixOf(
				resource.getLocation());
	}

	/**
	 * Add error markers from a list of error tuples
	 * 
	 * @param resource
	 * @param errorList
	 */
	public static void addErrorMarkers(IMarkerGenerator mg, IResource resource,
			OtpErlangList errorList) {
		for (int i = 0; i < errorList.arity(); i++) {
			final OtpErlangTuple data = (OtpErlangTuple) errorList.elementAt(i);

			final String msg = ((OtpErlangString) data.elementAt(2))
					.stringValue();
			int line = 0;
			if (data.elementAt(0) instanceof OtpErlangLong) {
				try {
					line = ((OtpErlangLong) data.elementAt(0)).intValue();
				} catch (final OtpErlangRangeException e) {
					;
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
				;
			}

			mg.addMarker(resource, msg, line, sev, "");
		}
	}

	private static void distributeModule(final String beamf,
			final OtpErlangBinary code) {
		BackendManager.getDefault().forEachRemote(new IBackendVisitor() {

			public void run(IBackend b) {
				try {
					if (b != BackendManager.getDefault().getIdeBackend()) {
						final RpcResult result = b.rpc("code", "load_binary",
								new OtpErlangAtom(beamf), new OtpErlangString(
										beamf), code);
						ErlLogger.debug("  $ distribute " + beamf + " to "
								+ b.getLabel() + "  - " + result.getValue());
					}
				} catch (final ErlangRpcException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		});
	}

	static class HrlDeltaVisitor implements IResourceDeltaVisitor {

		public boolean hasAny = false;

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.core.resources.IResourceDeltaVisitor#visit(org.eclipse.core.resources.IResourceDelta)
		 */
		public boolean visit(IResourceDelta delta) throws CoreException {
			final IResource resource = delta.getResource();
			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "hrl".compareTo(resource.getFileExtension()) == 0) {
				hasAny = true;
				return false;
			}

			// return true to continue visiting children.
			return true;
		}

	}

	class ErlangDeltaVisitor implements IResourceDeltaVisitor {

		private IProgressMonitor mon;

		public ErlangDeltaVisitor(IProgressMonitor submon) {
			mon = submon;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.core.resources.IResourceDeltaVisitor#visit(org.eclipse.core.resources.IResourceDelta)
		 */
		public boolean visit(IResourceDelta delta) throws CoreException {
			final IResource resource = delta.getResource();
			final IProject my_project = resource.getProject();
			final ErlangProjectProperties prefs = new ErlangProjectProperties(
					my_project);

			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "erl".equals(resource.getFileExtension())
					&& isInExtCodePath(resource, my_project)) {
				if (isDebugging()) {
					ErlLogger.debug("+++ Incr: " + resource.getName() + " "
							+ delta.getKind());
				}
				switch (delta.getKind()) {
				case IResourceDelta.ADDED:
					// handle added resource
					compileFile(my_project, resource);
					break;
				case IResourceDelta.REMOVED:
					// handle removed resource
					deleteMarkers((IFile) resource);

					IPath beam = new Path(prefs.getOutputDir());
					final IPath module = beam.append(resource.getName())
							.removeFileExtension();
					beam = module.addFileExtension("beam").setDevice(null);
					final IResource br = my_project.findMember(beam);
					if (br != null) {
						br.delete(true, null);
					}

					// was it derived from a yrl?
					final IPath yrlp = resource.getProjectRelativePath()
							.removeFileExtension().addFileExtension("yrl");
					final IResource yrl = my_project.findMember(yrlp);
					if (yrl != null) {
						compileYrlFile(my_project, yrl);
					}

					break;
				case IResourceDelta.CHANGED:
					// handle changed resource
					if (resource.isDerived()) {
						// TODO what do we do here?
						;
					} else {
						compileFile(my_project, resource);
					}
					break;
				}
				mon.worked(1);
			}
			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "yrl".equals(resource.getFileExtension())
					&& isInExtCodePath(resource, my_project)) {
				if (isDebugging()) {
					ErlLogger.debug("+++ Incr: " + resource.getName() + " "
							+ delta.getKind());
				}
				switch (delta.getKind()) {
				case IResourceDelta.ADDED:
					// handle added resource
					compileYrlFile(my_project, resource);
					break;
				case IResourceDelta.REMOVED:
					// handle removed resource
					deleteMarkers((IFile) resource);

					IPath erl = resource.getProjectRelativePath()
							.removeFileExtension();
					erl = erl.addFileExtension("erl").setDevice(null);
					final IResource br = my_project.findMember(erl);
					if (br != null) {
						br.delete(true, null);
					}
					break;
				case IResourceDelta.CHANGED:
					// handle changed resource
					compileYrlFile(my_project, resource);
					break;
				}
				mon.worked(1);
			}
			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "beam".equals(resource.getFileExtension())
					&& isInOutputPath(resource, my_project)) {
				switch (delta.getKind()) {
				case IResourceDelta.ADDED:
				case IResourceDelta.CHANGED:
					// what to do here?
					break;
				case IResourceDelta.REMOVED:
					final ErlangFileVisitor searcher = new ErlangFileVisitor(
							null);
					final String[] p = resource.getName().split("\\.");
					searcher.init(p[0]);
					my_project.accept(searcher);
					if (searcher.fResult != null) {
						ErlLogger.debug("recompiling " + searcher.fResult);
						compileFile(my_project, searcher.fResult);
					}
					break;
				}
				mon.worked(1);
			}
			// return true to continue visiting children.
			if (resource.getType() == IResource.FOLDER
					&& "org".equals(resource.getName())) {
				return false;
			} else {
				return true;
			}
		}
	}

	class ErlangResourceVisitor implements IResourceVisitor {

		private IProgressMonitor monitor;

		public ErlangResourceVisitor(IProgressMonitor subMon) {
			monitor = subMon;
		}

		public boolean visit(IResource resource) {
			final IProject my_project = resource.getProject();
			// final ErlangProjectProperties prefs = new
			// ErlangProjectProperties(my_project);

			// monitor.worked(1);
			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "erl".equals(resource.getFileExtension())
					&& isInExtCodePath(resource, my_project)) {

				try {
					if (isDebugging()) {
						ErlLogger.debug("Full: " + resource.getName());
					}
					// we should skip it is the beam file already exists and
					// timestamp is
					// recent
					final long timestamp = ((IFile) resource)
							.getLocalTimeStamp();
					long moduletimestamp = 0;

					final IErlProject prj = ErlangCore.getModel()
							.findErlangProject(resource.getProject());
					if (prj != null) {
						IErlModule mod = null;
						mod = prj.getModule(resource.getName());
						if (mod != null) {
							moduletimestamp = mod.getTimestamp();
						}
					}
					// if (timestamp > moduletimestamp)
					{
						if (isDebugging()) {
							ErlLogger.debug(" recompiling "
									+ resource.getName() + " " + timestamp
									+ " " + moduletimestamp);
						}
						compileFile(resource.getProject(), resource);
						monitor.worked(1);
					}
				} catch (final Exception e) {
					e.printStackTrace();
				}
			}
			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "yrl".equals(resource.getFileExtension())
					&& isInExtCodePath(resource, my_project)) {

				try {
					if (isDebugging()) {
						ErlLogger.debug("Full: " + resource.getName());
					}
					// we should skip it is the beam file already exists and
					// timestamp is
					// recent
					final long timestamp = ((IFile) resource)
							.getLocalTimeStamp();
					long erltimestamp = 0;

					IPath erl = resource.getProjectRelativePath()
							.removeFileExtension();
					erl = erl.addFileExtension("erl").setDevice(null);
					final IResource br = my_project.findMember(erl);
					if (br != null) {
						erltimestamp = br.getLocalTimeStamp();
					}
					// if (timestamp > moduletimestamp)
					{
						if (isDebugging()) {
							ErlLogger.debug(" recompiling "
									+ resource.getName() + " " + timestamp
									+ " " + erltimestamp);
						}
						compileYrlFile(resource.getProject(), resource);
						monitor.worked(1);
					}
				} catch (final Exception e) {
					e.printStackTrace();
				}
			}
			// return true to continue visiting children.
			if (resource.getType() == IResource.FOLDER
					&& "org".equals(resource.getName())) {
				return false;
			} else {
				return true;
			}
		}
	}

	static class ErlangFileVisitor implements IResourceVisitor {

		IResource fResult;

		String fName;

		public ErlangFileVisitor(IProgressMonitor submon) {
			fResult = null;
			fName = null;
		}

		public void init(String name) {
			fName = name;
			fResult = null;
		}

		public boolean visit(IResource resource) throws CoreException {
			if (fName == null) {
				return false;
			}
			if (fResult != null) {
				return false;
			}
			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "erl".equals(resource.getFileExtension())
					&& isInExtCodePath(resource, resource.getProject())) {
				final String[] p = resource.getName().split("\\.");
				if (p[0].equals(fName)) {
					fResult = resource;
					return false;
				}
			}
			return true;
		}
	}

	/**
	 * Compile Erlang module, given output directory and include directory
	 * 
	 * @throws ErlangRpcException
	 * @param fn
	 * @param outputdir
	 * @param includedirs
	 * @return OtpErlangObject
	 */
	protected static OtpErlangObject compileFile(IProject project, String fn,
			String outputdir, String[] includedirs) {
		if (isDebugging()) {
			ErlLogger.debug("!!! compiling " + fn);
		}
		try {
			final OtpErlangString[] includes = new OtpErlangString[includedirs.length];
			for (int i = 0; i < includedirs.length; i++) {
				includes[i] = new OtpErlangString(includedirs[i]);
			}
			final OtpErlangList includeList = new OtpErlangList(includes);
			return BackendManager.getDefault().get(project).rpcxt(MODULE,
					"compile", 20000, fn, outputdir, includeList
					// FIXME add an option for this
					, new OtpErlangList(new OtpErlangAtom("debug_info")));
		} catch (final Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	public static boolean isDebugging() {
		return ErlangPlugin.getDefault().isDebugging()
				&& Platform.getDebugOption("org.erlide.core/debug/builder")
						.equals("true");
	}

	protected static OtpErlangObject compileYrlFile(IProject project,
			String fn, String output) {
		if (isDebugging()) {
			ErlLogger.debug("!!! compiling " + fn);
		}
		try {
			final RpcResult r = BackendManager.getDefault().get(project).rpct(
					MODULE, "compile_yrl", 30000, fn, output);
			if (isDebugging()) {
				ErlLogger.debug("!!! r== " + r);
			}
			return r.getValue();
		} catch (final Exception e) {
			// e.printStackTrace();
			return null;
		}
	}

	protected static OtpErlangObject loadModule(IProject project, String module) {
		try {
			return BackendManager.getDefault().get(project).rpcx(MODULE,
					"load", new OtpErlangAtom(module));
		} catch (final BackendException e) {
			e.printStackTrace();
			return null;
		}
	}

	// /////////////////////////////////////////////////////

	IProject currentProject;

	IWorkspaceRoot workspaceRoot;

	BuildNotifier notifier;

	public static IMarker[] getProblemsFor(IResource resource) {
		try {
			if (resource != null && resource.exists()) {
				return resource.findMarkers(PROBLEM_MARKER, false,
						IResource.DEPTH_INFINITE);
			}
		} catch (final CoreException e) {
			// assume there are no problems
		}
		return new IMarker[0];
	}

	public static IMarker[] getTasksFor(IResource resource) {
		try {
			if (resource != null && resource.exists()) {
				return resource.findMarkers(TASK_MARKER, false,
						IResource.DEPTH_INFINITE);
			}
		} catch (final CoreException e) {
			// assume there are no tasks
		}
		return new IMarker[0];
	}

	/**
	 * Hook allowing to initialize some static state before a complete build
	 * iteration. This hook is invoked during PRE_AUTO_BUILD notification
	 */
	public static void buildStarting() {
		// build is about to start
	}

	/**
	 * Hook allowing to reset some static state after a complete build
	 * iteration. This hook is invoked during POST_AUTO_BUILD notification
	 */
	public static void buildFinished() {
		BuildNotifier.resetProblemCounters();
	}

	protected static void removeProblemsFor(IResource resource) {
		try {
			if (resource != null && resource.exists()) {
				resource.deleteMarkers(PROBLEM_MARKER, false,
						IResource.DEPTH_INFINITE);
			}
		} catch (final CoreException e) {
			// assume there were no problems
		}
	}

	protected static void removeTasksFor(IResource resource) {
		try {
			if (resource != null && resource.exists()) {
				resource.deleteMarkers(TASK_MARKER, false,
						IResource.DEPTH_INFINITE);
			}
		} catch (final CoreException e) {
			// assume there were no problems
		}
	}

	protected static void removeProblemsAndTasksFor(IResource resource) {
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

	@SuppressWarnings("unchecked")
	protected IProject[] build_1(int kind, Map ignored, IProgressMonitor monitor)
			throws CoreException {
		currentProject = getProject();
		if (currentProject == null || !currentProject.isAccessible()) {
			return new IProject[0];
		}

		if (isDebugging()) {
			System.out
					.println("\nStarting build of " + currentProject.getName() //$NON-NLS-1$
							+ " @ " + new Date(System.currentTimeMillis())); //$NON-NLS-1$
		}
		notifier = new BuildNotifier(monitor, currentProject);
		notifier.begin();
		try {
			notifier.checkCancel();
			initializeBuilder();

			if (kind == FULL_BUILD) {
				buildAll();
			} else {
				if (true) // hasStructuralDelta())
				{ // double check that a jar file didn't get replaced in a
					// binary project
					buildAll();
				} else {
					// buildDeltas();
				}
			}
		} catch (final CoreException e) {
			log(
					e,
					"JavaBuilder handling CoreException while building: " + currentProject.getName()); //$NON-NLS-1$
			final IMarker marker = currentProject.createMarker(PROBLEM_MARKER);
			marker.setAttribute(IMarker.MESSAGE, BuilderMessages.bind(
					BuilderMessages.build_inconsistentProject, e
							.getLocalizedMessage()));
			marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_ERROR);
		} finally {
			notifier.done();
			cleanup();
		}
		if (isDebugging()) {
			ErlLogger.debug("Finished build of " + currentProject.getName() //$NON-NLS-1$
					+ " @ " + new Date(System.currentTimeMillis())); //$NON-NLS-1$
		}
		return null;
	}

	private void buildAll() {
		notifier.checkCancel();
		notifier.subTask(BuilderMessages.build_preparingBuild);
		// fullBuild();
	}

	protected void clean_1(IProgressMonitor monitor) throws CoreException {
		currentProject = getProject();
		if (currentProject == null || !currentProject.isAccessible()) {
			return;
		}

		if (isDebugging()) {
			ErlLogger.debug("\nCleaning " + currentProject.getName() //$NON-NLS-1$
					+ " @ " + new Date(System.currentTimeMillis())); //$NON-NLS-1$
		}
		notifier = new BuildNotifier(monitor, currentProject);
		notifier.begin();
		try {
			notifier.checkCancel();

			initializeBuilder();
			removeProblemsAndTasksFor(currentProject);
			// new BatchImageBuilder(this).cleanOutputFolders(false);
		} catch (final CoreException e) {

			log(
					e,
					"JavaBuilder handling CoreException while cleaning: " + currentProject.getName()); //$NON-NLS-1$
			final IMarker marker = currentProject.createMarker(PROBLEM_MARKER);
			marker.setAttribute(IMarker.MESSAGE, BuilderMessages.bind(
					BuilderMessages.build_inconsistentProject, e
							.getLocalizedMessage()));
			marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_ERROR);
		} finally {
			notifier.done();
			cleanup();
		}
		if (isDebugging()) {
			ErlLogger.debug("Finished cleaning " + currentProject.getName() //$NON-NLS-1$
					+ " @ " + new Date(System.currentTimeMillis())); //$NON-NLS-1$
		}
	}

	private void cleanup() {
		notifier = null;
	}

	private void initializeBuilder() throws CoreException {
		workspaceRoot = currentProject.getWorkspace().getRoot();
	}

	public void log(Throwable e, String message) {
		Throwable nestedException;
		if (e instanceof ErlModelException
				&& (nestedException = ((ErlModelException) e).getException()) != null) {
			e = nestedException;
		}
		final IStatus status = new Status(IStatus.ERROR,
				ErlangPlugin.PLUGIN_ID, IStatus.ERROR, message, e);
		ErlangPlugin.getDefault().getLog().log(status);
	}

	public static Object readState(IProject project, DataInputStream in) {
		return null;
	}

	@SuppressWarnings("unchecked")
	protected IProject[] build_remote(int kind, Map args,
			IProgressMonitor monitor) throws CoreException {
		monitor.beginTask("Remote building", 100);
		try {
			// args.put("server", "gsnbuild1");
			// args.put("remoteuser", "qvladum");
			// args.put("password", "");

			ErlLogger.debug("## BUILDING!!! " + args.toString());

			final String server = (String) args.get("server");
			final String localuser = (String) args.get("remoteuser");
			final String remoteuser = (String) args.get("remoteuser");
			final String terminal = "network";

			ErlLogger
					.debug("## " + server + "." + localuser + "." + remoteuser);

			if (server == null || localuser == null || remoteuser == null) {
				return null;
				// throw new CoreException(new Status(IStatus.ERROR,
				// ErlangPlugin.PLUGIN_ID,
				// 1,
				// "build failed: bad or no arguments", null));
			}

			RLoginClient client;

			client = new RLoginClient();

			try {
				client.connect(server);
			} catch (final IOException e) {
				throw new CoreException(new Status(IStatus.ERROR,
						ErlangPlugin.PLUGIN_ID, 1,
						"build failed: Could not connect to server.", e));
			}

			try {
				client.rlogin(localuser, remoteuser, terminal);
			} catch (final IOException e) {
				try {
					client.disconnect();
				} catch (final IOException f) {
				}
				throw new CoreException(new Status(IStatus.ERROR,
						ErlangPlugin.PLUGIN_ID, 1,
						"build failed: rlogin authentication failed.", e));
			}

			final String pass = args.get("password") + "\n";

			final RemoteConnector rconn = new RemoteConnector();

			final ByteArrayOutputStream out = new ByteArrayOutputStream(1024);

			rconn.connectReadWrite(client.getInputStream(), client
					.getOutputStream(), null, out);

			rconn.write(pass);
			rconn.write("4\n");
			rconn.write("ls\nls -l\ncleartool lsview | grep " + remoteuser
					+ "\n");

			rconn.write("exit\n");
			rconn.waitDisconnect();

			try {
				out.close();
			} catch (final IOException e1) {
				// e1.printStackTrace();
			}

			ErlLogger.debug(out.toString());

			try {
				client.disconnect();
			} catch (final IOException e) {
				e.printStackTrace();
			}
		} finally {
			monitor.done();
		}
		return null;
	}

	protected void clean_remote(IProgressMonitor monitor) throws CoreException {
		super.clean(monitor);
	}

	// private void scheduleRefresh()
	// {
	// Job job = new Job("Refreshing resources...") {
	//
	// public IStatus run(final IProgressMonitor monitor)
	// {
	// IStatus result = Status.OK_STATUS;
	// try
	// {
	// getProject().refreshLocal(IResource.DEPTH_INFINITE, monitor);
	// } catch (CoreException cex)
	// {
	// String msg = "Problem during resource refresh after build.";
	// ErlangPlugin.log(msg, cex);
	// result = cex.getStatus();
	// }
	// return result;
	// }
	// };
	// job.schedule();
	// }

	public void addMarker(IResource file, String errorDesc, int lineNumber,
			int severity, String errorVar) {
		addMarker(file, errorDesc, lineNumber, severity);
	}

}
