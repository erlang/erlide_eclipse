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
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.apache.commons.net.bsd.RLoginClient;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.IMarkerGenerator;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlComment;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IErlScanner;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.core.erlang.IErlModule.ModuleKind;
import org.erlide.core.util.ErlangIncludeFile;
import org.erlide.core.util.RemoteConnector;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.ErlangProjectProperties;
import org.erlide.runtime.backend.BuildBackend;
import org.erlide.runtime.backend.IBackend;
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

import erlang.ErlangCode;
import erlang.ErlideBuilder;

/**
 * @author Vlad Dumitrescu
 */
public class ErlangBuilder extends IncrementalProjectBuilder implements
		IMarkerGenerator {

	protected static final String PROBLEM_MARKER = ErlangPlugin.PLUGIN_ID
			+ ".problemmarker";

	protected static final String TASK_MARKER = ErlangPlugin.PLUGIN_ID
			+ ".taskmarker";

	// TODO how do we configure builder?
	// use the internal builder
	private final boolean fInternal = true;

	static class ErlangBuilderMarkerGenerator implements IMarkerGenerator {

		public void addMarker(final IResource file,
				final IResource compiledFile, final String errorDesc,
				final int lineNumber, final int severity, final String errorVar) {
			ErlangBuilder.addProblemMarker(file, compiledFile, errorDesc,
					lineNumber, severity);
		}
	};

	static private IMarkerGenerator getMarkerGenerator() {
		return new ErlangBuilder.ErlangBuilderMarkerGenerator();
	}

	static void addProblemMarker(final IResource file,
			final IResource compiledFile, final String message, int lineNumber,
			final int severity) {
		try {
			final IMarker marker = file.createMarker(PROBLEM_MARKER);
			marker.setAttribute(IMarker.MESSAGE, message);
			marker.setAttribute(IMarker.SEVERITY, severity);
			marker.setAttribute(IMarker.SOURCE_ID, compiledFile.getFullPath()
					.toString());
			if (lineNumber == -1) {
				lineNumber = 1;
			}
			marker.setAttribute(IMarker.LINE_NUMBER, lineNumber);
		} catch (final CoreException e) {
		}
	}

	static void addTaskMarker(final IResource file,
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

	protected void createProblemFor(final IResource resource,
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

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.internal.events.InternalBuilder#build(int,
	 * java.util.Map, org.eclipse.core.runtime.IProgressMonitor)
	 */
	@SuppressWarnings("unchecked")
	@Override
	protected IProject[] build(final int kind, final Map args,
			final IProgressMonitor monitor) throws CoreException {
		currentProject = getProject();
		if (currentProject == null || !currentProject.isAccessible()) {
			return new IProject[0];
		}
		ErlangProjectProperties pp = new ErlangProjectProperties(currentProject);
		if (pp.isReference()) {
			return null;
		}

		if (BuilderUtils.isDebugging()) {
			ErlLogger.debug("Starting build of " + currentProject.getName() //$NON-NLS-1$
					+ " @ " + new Date(System.currentTimeMillis()));
		}
		notifier = new BuildNotifier(monitor, currentProject);
		notifier.begin();
		try {
			initializeBuilder();

			if (fInternal) {
				if (kind == FULL_BUILD) {
					fullBuild(args, notifier);
				} else {
					final IResourceDelta delta = getDelta(getProject());
					if (hasHrl(delta)) {
						fullBuild(args, notifier);
					} else {
						incrementalBuild(args, delta, notifier);
					}
				}
			} else {
				build_remote(kind, args, notifier);
			}

			try {
				checkForClashes();
			} catch (final Exception e) {
			}

		} catch (final CoreException e) {
			log(e, "JavaBuilder handling CoreException while building: "
					+ currentProject.getName());
			final IMarker marker = currentProject.createMarker(PROBLEM_MARKER);
			marker.setAttribute(IMarker.MESSAGE, BuilderMessages.bind(
					BuilderMessages.build_inconsistentProject, e
							.getLocalizedMessage()));
			marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_ERROR);
		} finally {
			notifier.done();
			cleanup();
		}
		if (BuilderUtils.isDebugging()) {
			ErlLogger.debug("Finished build of " + currentProject.getName() //$NON-NLS-1$
					+ " @ " + new Date(System.currentTimeMillis()));
		}
		return null;
	}

	private void checkForClashes() throws BackendException {
		// try {
		// getProject().deleteMarkers(PROBLEM_MARKER, true,
		// IResource.DEPTH_ZERO);
		// } catch (final CoreException e1) {
		// }

		final BuildBackend b = ErlangCore.getBackendManager().getBuildBackend(
				getProject());
		try {
			final OtpErlangList res = ErlideBuilder.getCodeClashes(b);
			for (int i = 0; i < res.arity(); i++) {
				final OtpErlangTuple t = (OtpErlangTuple) res.elementAt(i);
				final String f1 = ((OtpErlangString) t.elementAt(0))
						.stringValue();
				final String f2 = ((OtpErlangString) t.elementAt(1))
						.stringValue();

				// add marker only for modules belonging to this project!
				final IProject p = getProject();
				final IResource r1 = p.findMember(f1);
				final IResource r2 = p.findMember(f2);
				// XXX does the above work? or do we need to get the name only?
				if (r1 != null || r2 != null) {
					addMarker(getProject(), getProject(), "Code clash between "
							+ f1 + " and " + f2, 0, IMarker.SEVERITY_WARNING,
							"");
				}
			}

		} catch (final Exception e) {
		}
		try {
			final ErlangProjectProperties pp = new ErlangProjectProperties(
					getProject());
			final String[] sd = pp.getSourceDirs();
			final String[] dirList = new String[sd.length];
			for (int i = 0; i < sd.length; i++) {
				dirList[i] = getProject().getLocation().toPortableString()
						+ "/" + sd[i];
			}
			final OtpErlangList res = ErlideBuilder
					.getSourceClashes(b, dirList);
			for (int i = 0; i < res.arity(); i++) {
				final OtpErlangTuple t = (OtpErlangTuple) res.elementAt(i);
				final String f1 = ((OtpErlangString) t.elementAt(0))
						.stringValue();
				final String f2 = ((OtpErlangString) t.elementAt(1))
						.stringValue();
				addMarker(getProject(), getProject(),
						"Duplicated module name in " + f1 + " and " + f2, 0,
						IMarker.SEVERITY_ERROR, "");
			}
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

	private boolean hasHrl(final IResourceDelta delta) {
		if (delta == null
				|| !((delta.getKind() == IResourceDelta.ADDED) || (delta
						.getKind() == IResourceDelta.CHANGED))) {
			return false;
		}

		final IResource resource = delta.getResource();
		if (resource.getType() == IResource.FILE
				&& resource.getFileExtension() != null
				&& "hrl".compareTo(resource.getFileExtension()) == 0) {
			return true;

		}

		IResourceDelta[] children = delta
				.getAffectedChildren(IResourceDelta.ADDED
						| IResourceDelta.CHANGED);
		for (IResourceDelta child : children) {
			if (hasHrl(child)) {
				return true;
			}
		}

		return false;
	}

	protected void deleteMarkers(final IResource resource) {
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

	private void deleteMarkersWithCompiledFile(final IProject project,
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

	@SuppressWarnings("unchecked")
	protected void fullBuild(final Map args, final BuildNotifier notifier)
			throws CoreException {
		final int n = getErlangResourcesCount(getProject());
		if (BuilderUtils.isDebugging()) {
			ErlLogger.debug("full build..." + getProject() + " " + n);
		}

		notifier.setProgressPerCompilationUnit(1.0f / n);
		try {
			getProject().accept(new ErlangResourceVisitor(notifier));
		} finally {
			notifier.done();
		}
	}

	private int getErlangResourcesCount(final IProject project) {
		final ErlangProjectProperties prefs = new ErlangProjectProperties(
				project);
		final String[] dirs = prefs.getSourceDirs();
		int n = 0;
		for (final String dir : dirs) {
			final IFile f = project.getFile(dir);
			final String dirstr = f.getLocation().toString();
			final String[] list = new File(dirstr).list();
			n += list == null ? 0 : list.length;
		}
		return n;
	}

	@SuppressWarnings("unchecked")
	protected void incrementalBuild(final Map args, final IResourceDelta delta,
			final BuildNotifier notifier) throws CoreException {
		if (BuilderUtils.isDebugging()) {
			ErlLogger.debug("incr build...");
		}
		final IResourceDelta[] chd = delta.getAffectedChildren();
		final int n = chd.length;
		notifier.setProgressPerCompilationUnit(1.0f / n);
		try {
			// the visitor does the work.
			delta.accept(new ErlangDeltaVisitor(notifier));
		} finally {
			notifier.done();
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
	protected void clean(final IProgressMonitor monitor) throws CoreException {
		if (BuilderUtils.isDebugging()) {
			ErlLogger.debug("cleaning... " + getProject());
		}

		super.clean(monitor);
		getProject().deleteMarkers(PROBLEM_MARKER, true,
				IResource.DEPTH_INFINITE);
		getProject().deleteMarkers(TASK_MARKER, true, IResource.DEPTH_INFINITE);

		// TODO also delete beam files

		final ErlangProjectProperties prefs = new ErlangProjectProperties(
				getProject());

		// delete beam files
		final IFolder bf = getProject().getFolder(prefs.getOutputDir());
		try {
			final IResource[] beams = bf.members();
			for (final IResource element : beams) {
				if ("beam".equals(element.getFileExtension())) {
					element.delete(true, monitor);
				}
			}
		} catch (final CoreException e) {
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
	protected void compileFile(final IProject project, final IResource resource) {
		final IPath projectPath = project.getLocation();
		final ErlangProjectProperties prefs = new ErlangProjectProperties(
				project);

		final String s = resource.getFileExtension();
		if (!s.equals("erl")) {
			System.out.println("?!?!");
		}

		deleteMarkers(resource);
		if (isInExtCodePath(resource, project)
				&& !isInCodePath(resource, project)) {
			addMarker(
					resource,
					resource,
					resource.getName()
							+ " is not directly on source path (packages are not supported yet)",
					0, IMarker.SEVERITY_WARNING, "");
			return;
		}

		final String outputDir = projectPath.append(prefs.getOutputDir())
				.toString();
		ensureDirExists(outputDir);

		List<String> includeDirs = getIncludeDirs(project,
				new ArrayList<String>());

		try {
			final IProject[] referencedProjects = project
					.getReferencedProjects();
			for (final IProject p : referencedProjects) {
				if (p.isAccessible()) {
					includeDirs = getIncludeDirs(p, includeDirs);
				}
			}
		} catch (final CoreException e1) {
		}

		// delete beam file
		IPath beam = new Path(prefs.getOutputDir());
		final IPath module = beam.append(resource.getName())
				.removeFileExtension();
		beam = module.addFileExtension("beam").setDevice(null);
		IResource br = project.findMember(beam);

		try {
			boolean shouldCompile = br == null;

			if (br != null) {
				final IErlProject eprj = ErlangCore.getModel().findProject(
						project);
				if (eprj != null) {
					final IErlModule m = eprj.getModule(resource.getName());
					if (m != null) {
						final List<ErlangIncludeFile> incs = m
								.getIncludedFiles();
						for (final ErlangIncludeFile ifile : incs) {
							final IResource rifile = findResourceByName(
									project, ifile.getFilename());
							if (rifile != null
									&& rifile.getLocalTimeStamp() > br
											.getLocalTimeStamp()) {
								shouldCompile = true;
								break;
							}
						}
					}
				}
			}

			if (br != null) {
				shouldCompile |= br.getLocalTimeStamp() < resource
						.getLocalTimeStamp();
			}

			if (shouldCompile) {
				if (br != null) {
					br.delete(true, null);
				}

				createTaskMarkers(project, resource);

				if (BuilderUtils.isDebugging()) {
					ErlLogger.debug("compiling %s", resource.getName());
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
					// final OtpErlangBinary code = (OtpErlangBinary) t
					// .elementAt(2);
					// for (IBackend b :
					// BackendManager.getDefault().getExecution(
					// project)) {
					// distributeModule(b, beamf, code);
					// }
					ErlideBuilder.loadModule(project, beamf);
				} else {
					ErlLogger.debug(">>>> compile error..."
							+ resource.getName());
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

				// ErlLogger.debug("t = " + t);
				// process compilation messages
				final OtpErlangList l = (OtpErlangList) t.elementAt(1);
				addErrorMarkers(getMarkerGenerator(), resource, l);
			} else {
				if (BuilderUtils.isDebugging()) {
					ErlLogger.debug("skipping %s", resource.getName());
				}
			}
		} catch (final Exception e) {
			ErlLogger.warn(e);
		}

	}

	/**
	 * @param project
	 * @param prefs
	 * @return
	 */
	private List<String> getIncludeDirs(final IProject project,
			final List<String> includeDirs) {
		final ErlangProjectProperties prefs = new ErlangProjectProperties(
				project);
		final String[] incs = prefs.getIncludeDirs();
		final IPathVariableManager pvm = ResourcesPlugin.getWorkspace()
				.getPathVariableManager();
		for (int i = 0; i < incs.length; i++) {
			final IPath inc = pvm.resolvePath(new Path(incs[i]));
			if (inc.isAbsolute()) {
				includeDirs.add(inc.toString());
			} else {
				final IFolder folder = project.getFolder(incs[i]);
				if (folder != null) {
					final IPath location = folder.getLocation();
					includeDirs.add(location.toString());
				}
			}
		}
		return includeDirs;
	}

	private void createTaskMarkers(final IProject project,
			final IResource resource) {
		final IErlProject p = ErlangCore.getModel().findProject(project);
		if (p != null) {
			try {
				final IErlModule m = p.getModule(resource.getName());
				if (m == null) {
					return;
				}
				final IErlScanner s = m.getScanner();
				final List<IErlComment> cl = s.getComments();
				for (final IErlComment c : cl) {
					final String name = c.getName();
					mkMarker(resource, c, name, "TODO", IMarker.PRIORITY_NORMAL);
					mkMarker(resource, c, name, "XXX", IMarker.PRIORITY_NORMAL);
					mkMarker(resource, c, name, "FIXME", IMarker.PRIORITY_HIGH);
				}
				// TODO we don't want all of the scanner data to linger around
				// but disposing might delete a scanner that is used...
				// TODO we need to reference count on the erlang side!
				// s.dispose();
			} catch (final ErlModelException e) {
			}
		}

	}

	@SuppressWarnings("unused")
	private List<IErlComment> getComments(final IResource resource) {
		final List<IErlComment> result = new ArrayList<IErlComment>();
		return result;
	}

	private void mkMarker(final IResource resource, final IErlComment c,
			final String name, final String tag, final int prio) {
		if (name.contains(tag)) {
			final int ix = name.indexOf(tag);
			final String msg = name.substring(ix);
			int dl = 0;
			for (int i = 0; i < ix; i++) {
				if (name.charAt(i) == '\n') {
					dl++;
				}
			}
			addTaskMarker(resource, resource, msg, c.getLineStart() + 1 + dl,
					prio);
		}
	}

	private void ensureDirExists(final String outputDir) {
		final File f = new File(outputDir);
		f.mkdir();
	}

	protected void compileYrlFile(final IProject project,
			final IResource resource) {
		// final IPath projectPath = project.getLocation();
		// final ErlangProjectProperties prefs = new
		// ErlangProjectProperties(project);

		deleteMarkers(resource);
		// try {
		// resource.deleteMarkers(PROBLEM_MARKER, true,
		// IResource.DEPTH_INFINITE);
		// } catch (final CoreException e1) {
		// }
		if (isInExtCodePath(resource, project)
				&& !isInCodePath(resource, project)) {
			addMarker(
					resource,
					resource,
					resource.getName()
							+ " is not directly on source path (packages are not supported yet)",
					0, IMarker.SEVERITY_WARNING, "");
			return;
		}

		IPath erl = resource.getProjectRelativePath().removeFileExtension();
		erl = erl.addFileExtension("erl").setDevice(null);
		IResource br = project.findMember(erl);

		// TODO check timestamps!

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

	public static boolean isInCodePath(final IResource resource,
			final IProject project) {
		final ErlangProjectProperties prefs = new ErlangProjectProperties(
				project);
		final IPath projectPath = project.getFullPath();
		final String[] srcs = prefs.getSourceDirs();
		final IPath exceptLastSegment = resource.getFullPath()
				.removeLastSegments(1);
		for (final String element : srcs) {
			final IPath sp = projectPath.append(new Path(element));
			if (sp.equals(exceptLastSegment)) {
				return true;
			}
		}

		return false;
	}

	static boolean isInExtCodePath(final IResource resource,
			final IProject project) {
		final ErlangProjectProperties prefs = new ErlangProjectProperties(
				project);
		final IPath projectPath = project.getFullPath();
		final String[] srcs = prefs.getSourceDirs();
		final IPath fullPath = resource.getFullPath();
		for (final String element : srcs) {
			final IPath sp = projectPath.append(new Path(element));
			if (sp.isPrefixOf(fullPath)) {
				return true;
			}
		}

		return false;
	}

	boolean isInIncludedPath(final IResource resource, final IProject my_project) {
		final List<String> inc = new ArrayList<String>();
		getIncludeDirs(my_project, inc);

		for (final String s : inc) {
			final IPath p = new Path(s);
			final IPath resourcePath = resource.getLocation();
			if (p.isPrefixOf(resourcePath)) {
				return true;
			}
		}
		return false;
	}

	static boolean isInOutputPath(final IResource resource,
			final IProject project) {
		final ErlangProjectProperties prefs = new ErlangProjectProperties(
				project);
		final IPath projectPath = project.getLocation();

		final String out = prefs.getOutputDir();
		return projectPath.append(new Path(out)).isPrefixOf(
				resource.getLocation());
	}

	private static boolean comparePath(final String p1, final String p2) {
		final boolean WINDOWS = java.io.File.separatorChar == '\\';
		if (WINDOWS) {
			return p1.equalsIgnoreCase(p2);
		} else {
			return p1.equals(p2);
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
		for (int i = 0; i < errorList.arity(); i++) {
			final OtpErlangTuple data = (OtpErlangTuple) errorList.elementAt(i);

			final String msg = ((OtpErlangString) data.elementAt(2))
					.stringValue();
			final String fileName = ((OtpErlangString) data.elementAt(1))
					.stringValue();
			IResource res = resource;
			if (!comparePath(resource.getLocation().toString(), fileName)) {
				res = findResource(resource.getProject(), fileName);
				if (res == null) {
					res = resource;
				}
			}
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

			mg.addMarker(res, resource, msg, line, sev, "");
		}
	}

	private static IResource findResource(final IContainer container,
			final String fileName) {
		try {
			for (final IResource r : container.members()) {
				if (comparePath(r.getLocation().toString(), fileName)) {
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

	private static IResource findResourceByName(final IContainer container,
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

	@SuppressWarnings("unused")
	private static void distributeModule(final IBackend b, final String beamf,
			final OtpErlangBinary code) {
		if (b == null) {
			return;
		}
		try {
			final RpcResult result = ErlangCode.loadBinary(b, beamf, code);
			ErlLogger.debug(" $ distribute " + beamf + " to "
					+ b.getInfo().getName() + " - " + result.getValue());
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}

	class ErlangDeltaVisitor implements IResourceDeltaVisitor {

		private final BuildNotifier mon;

		public ErlangDeltaVisitor(final BuildNotifier notifier) {
			mon = notifier;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.eclipse.core.resources.IResourceDeltaVisitor#visit(org.eclipse
		 * .core.resources.IResourceDelta)
		 */
		public boolean visit(final IResourceDelta delta) throws CoreException {

			final IResource resource = delta.getResource();
			final IProject my_project = resource.getProject();
			final ErlangProjectProperties prefs = new ErlangProjectProperties(
					my_project);

			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "erl".equals(resource.getFileExtension())
					&& isInExtCodePath(resource, my_project)) {
				if (BuilderUtils.isDebugging()) {
					ErlLogger.debug("+++ Incr: " + resource.getName() + " "
							+ delta.getKind());
				}
				switch (delta.getKind()) {
				case IResourceDelta.ADDED:
				case IResourceDelta.CHANGED:
					// handle changed resource
					if (!resource.isDerived()) {
						mon.aboutToCompile(resource);
						compileFile(my_project, resource);
						mon.compiled(resource);
					}
					break;
				case IResourceDelta.REMOVED:
					// handle removed resource
					deleteMarkers(resource);

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
						mon.aboutToCompile(resource);
						compileYrlFile(my_project, yrl);
						mon.compiled(resource);
					}

					break;
				}
			}
			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "hrl".equals(resource.getFileExtension())
					&& isInIncludedPath(resource, my_project)) {
				if (BuilderUtils.isDebugging()) {
					ErlLogger.debug("+++ Incr: " + resource.getName() + " "
							+ delta.getKind());
				}
				switch (delta.getKind()) {
				case IResourceDelta.ADDED:
					break;
				case IResourceDelta.REMOVED:
				case IResourceDelta.CHANGED:
					checkDependents(resource, my_project, notifier);
					break;
				}
			}
			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "yrl".equals(resource.getFileExtension())
					&& isInExtCodePath(resource, my_project)) {
				if (BuilderUtils.isDebugging()) {
					ErlLogger.debug("+++ Incr: " + resource.getName() + " "
							+ delta.getKind());
				}
				switch (delta.getKind()) {
				case IResourceDelta.ADDED:
				case IResourceDelta.CHANGED:
					mon.aboutToCompile(resource);
					compileYrlFile(my_project, resource);
					mon.compiled(resource);
					break;

				case IResourceDelta.REMOVED:
					deleteMarkers(resource);

					IPath erl = resource.getProjectRelativePath()
							.removeFileExtension();
					erl = erl.addFileExtension("erl").setDevice(null);
					final IResource br = my_project.findMember(erl);
					if (br != null) {
						br.delete(true, null);
					}
					break;
				}
			}
			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "beam".equals(resource.getFileExtension())
					&& isInOutputPath(resource, my_project)) {
				switch (delta.getKind()) {
				case IResourceDelta.ADDED:
				case IResourceDelta.CHANGED:
					break;
				case IResourceDelta.REMOVED:
					final ErlangFileVisitor searcher = new ErlangFileVisitor(
							null);
					final String[] p = resource.getName().split("\\.");
					searcher.init(p[0]);
					my_project.accept(searcher);
					if (searcher.fResult != null) {
						mon.aboutToCompile(resource);
						compileFile(my_project, searcher.fResult);
						mon.compiled(resource);
					}
					break;
				}
			}
			// return true to continue visiting children.
			if (resource.getType() == IResource.FOLDER
					&& "org".equals(resource.getName())) {
				return false;
			}
			return true;
		}

	}

	void checkDependents(final IResource resource, final IProject my_project,
			final BuildNotifier mon) throws ErlModelException {
		final IErlProject eprj = ErlangCore.getModel().findProject(my_project);
		if (eprj != null) {
			final List<IErlModule> ms = eprj.getModules();
			for (final IErlModule m : ms) {
				final List<ErlangIncludeFile> incs = m.getIncludedFiles();
				for (final ErlangIncludeFile ifile : incs) {
					if (comparePath(ifile.getFilename(), resource.getName())) {
						if (m.getModuleKind() == ModuleKind.ERL) {
							mon.aboutToCompile(m.getResource());
							compileFile(my_project, m.getResource());
							mon.compiled(m.getResource());
						}
						break;
					}
				}
			}
		}
	}

	class ErlangResourceVisitor implements IResourceVisitor {

		private final BuildNotifier monitor;

		public ErlangResourceVisitor(final BuildNotifier notifier) {
			monitor = notifier;
		}

		public boolean visit(final IResource resource) throws CoreException {
			final IProject my_project = resource.getProject();

			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "erl".equals(resource.getFileExtension())
					&& isInExtCodePath(resource, my_project)
					&& !resource.isDerived()) {

				try {
					monitor.aboutToCompile(resource);
					compileFile(resource.getProject(), resource);
					monitor.compiled(resource);
				} catch (final Exception e) {
					e.printStackTrace();
				}
			}
			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "hrl".equals(resource.getFileExtension())
					&& isInIncludedPath(resource, my_project)) {
				checkDependents(resource, my_project, notifier);
			}
			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "yrl".equals(resource.getFileExtension())
					&& isInExtCodePath(resource, my_project)) {

				try {
					monitor.aboutToCompile(resource);
					compileYrlFile(resource.getProject(), resource);
					monitor.compiled(resource);
				} catch (final Exception e) {
					e.printStackTrace();
				}
			}
			// return true to continue visiting children.
			if (resource.getType() == IResource.FOLDER
					&& "org".equals(resource.getName())) {
				return false;
			}
			return true;
		}
	}

	static class ErlangFileVisitor implements IResourceVisitor {

		IResource fResult;

		String fName;

		public ErlangFileVisitor(final IProgressMonitor submon) {
			fResult = null;
			fName = null;
		}

		public void init(final String name) {
			fName = name;
			fResult = null;
		}

		public boolean visit(final IResource resource) throws CoreException {
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
	protected static OtpErlangObject compileFile(final IProject project,
			final String fn, final String outputdir,
			final List<String> includedirs) {
		if (BuilderUtils.isDebugging()) {
			ErlLogger.debug("!!! compiling " + fn);
		}
		return ErlideBuilder.compileErl(project, fn, outputdir, includedirs);
	}

	protected static OtpErlangObject compileYrlFile(final IProject project,
			final String fn, final String output) {
		if (BuilderUtils.isDebugging()) {
			ErlLogger.debug("!!! compiling " + fn);
		}
		return ErlideBuilder.compileYrl(project, fn, output);
	}

	IProject currentProject;

	IWorkspaceRoot workspaceRoot;

	BuildNotifier notifier;

	public static IMarker[] getProblemsFor(final IResource resource) {
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

	public static IMarker[] getTasksFor(final IResource resource) {
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

	protected static void removeProblemsFor(final IResource resource) {
		try {
			if (resource != null && resource.exists()) {
				resource.deleteMarkers(PROBLEM_MARKER, false,
						IResource.DEPTH_INFINITE);
			}
		} catch (final CoreException e) {
			// assume there were no problems
		}
	}

	protected static void removeTasksFor(final IResource resource) {
		try {
			if (resource != null && resource.exists()) {
				resource.deleteMarkers(TASK_MARKER, false,
						IResource.DEPTH_INFINITE);
			}
		} catch (final CoreException e) {
			// assume there were no problems
		}
	}

	protected static void removeProblemsAndTasksFor(final IResource resource) {
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

	protected void clean_1(final IProgressMonitor monitor) throws CoreException {
		currentProject = getProject();
		if (currentProject == null || !currentProject.isAccessible()) {
			return;
		}

		if (BuilderUtils.isDebugging()) {
			ErlLogger.debug("\nCleaning " + currentProject.getName() //$NON-NLS-1$
					+ " @ " + new Date(System.currentTimeMillis()));
		}
		notifier = new BuildNotifier(monitor, currentProject);
		notifier.begin();
		try {
			notifier.checkCancel();

			initializeBuilder();
			removeProblemsAndTasksFor(currentProject);
			// new BatchImageBuilder(this).cleanOutputFolders(false);
		} catch (final CoreException e) {

			log(e, "JavaBuilder handling CoreException while cleaning: "
					+ currentProject.getName());
			final IMarker marker = currentProject.createMarker(PROBLEM_MARKER);
			marker.setAttribute(IMarker.MESSAGE, BuilderMessages.bind(
					BuilderMessages.build_inconsistentProject, e
							.getLocalizedMessage()));
			marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_ERROR);
		} finally {
			notifier.done();
			cleanup();
		}
		if (BuilderUtils.isDebugging()) {
			ErlLogger.debug("Finished cleaning " + currentProject.getName() //$NON-NLS-1$
					+ " @ " + new Date(System.currentTimeMillis()));
		}
	}

	private void cleanup() {
		notifier = null;
	}

	private void initializeBuilder() throws CoreException {
		workspaceRoot = currentProject.getWorkspace().getRoot();
	}

	public void log(Throwable e, final String message) {
		Throwable nestedException;
		if (e instanceof ErlModelException
				&& (nestedException = ((ErlModelException) e).getException()) != null) {
			e = nestedException;
		}
		final IStatus status = new Status(IStatus.ERROR,
				ErlangPlugin.PLUGIN_ID, IStatus.ERROR, message, e);
		ErlangPlugin.getDefault().getLog().log(status);
	}

	public static Object readState(final IProject project,
			final DataInputStream in) {
		return null;
	}

	@SuppressWarnings("unchecked")
	protected IProject[] build_remote(final int kind, final Map args,
			final BuildNotifier notifier) throws CoreException {
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
			notifier.done();
		}
		return null;
	}

	protected void clean_remote(final IProgressMonitor monitor)
			throws CoreException {
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

	public void addMarker(final IResource file, final IResource compiledFile,
			final String errorDesc, final int lineNumber, final int severity,
			final String errorVar) {
		addProblemMarker(file, compiledFile, errorDesc, lineNumber, severity);
	}

}
