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
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.net.bsd.RLoginClient;
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
import org.erlide.core.ErlangProjectProperties;
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
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.Backend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlangCode;
import erlang.ErlideBuilder;

public class ErlangBuilder extends IncrementalProjectBuilder {

	protected static final String PROBLEM_MARKER = ErlangPlugin.PLUGIN_ID
			+ ".problemmarker";

	protected static final String TASK_MARKER = ErlangPlugin.PLUGIN_ID
			+ ".taskmarker";

	private IMarkerGenerator generator = new ErlangBuilderMarkerGenerator();
	IProject currentProject;
	IWorkspaceRoot workspaceRoot;
	BuildNotifier notifier;

	@Override
	protected void clean(final IProgressMonitor monitor) throws CoreException {
		currentProject = getProject();
		if (currentProject == null || !currentProject.isAccessible()) {
			return;
		}

		if (BuilderUtils.isDebugging()) {
			ErlLogger.debug("\nCleaning " + currentProject.getName() //$NON-NLS-1$
					+ " @ " + new Date(System.currentTimeMillis()));
		}
		super.clean(monitor);

		notifier = new BuildNotifier(monitor, currentProject);
		notifier.begin();
		try {
			notifier.checkCancel();

			initializeBuilder();
			removeProblemsAndTasksFor(currentProject);

			final ErlangProjectProperties prefs = ErlangCore
					.getProjectProperties(currentProject);
			final IFolder bf = currentProject.getFolder(prefs.getOutputDir());
			if (bf.exists()) {
				final IResource[] beams = bf.members();
				monitor.beginTask("Cleaning Erlang files", beams.length);
				for (final IResource element : beams) {
					if ("beam".equals(element.getFileExtension())) {
						element.delete(true, monitor);
						monitor.worked(1);
					}
				}
			}

		} catch (final CoreException e) {
			ErlLogger.error(e);
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

	@SuppressWarnings("unchecked")
	@Override
	protected IProject[] build(final int kind, final Map args,
			final IProgressMonitor monitor) throws CoreException {
		currentProject = getProject();
		if (currentProject == null || !currentProject.isAccessible()) {
			return new IProject[0];
		}

		if (BuilderUtils.isDebugging()) {
			ErlLogger.debug("Starting build of " + currentProject.getName() //$NON-NLS-1$
					+ " @ " + new Date(System.currentTimeMillis()));
		}
		BuildNotifier.resetProblemCounters();
		notifier = new BuildNotifier(monitor, currentProject);
		notifier.begin();
		try {
			initializeBuilder();

			Set<IResource> resourcesToBuild = new HashSet<IResource>();
			if (kind == FULL_BUILD) {
				resourcesToBuild = fullBuild(args);
			} else {
				final IResourceDelta delta = getDelta(currentProject);
				Path path = new Path(".settings/org.erlide.core.prefs");
				if (delta.findMember(path) != null) {
					ErlLogger
							.info("project configuration changed: doing full rebuild");
					resourcesToBuild = fullBuild(args);
				} else {
					resourcesToBuild = incrementalBuild(args, delta);
				}
			}
			final int n = resourcesToBuild.size();
			if (BuilderUtils.isDebugging()) {
				ErlLogger.debug("Will compile %d resource(s): %s", new Integer(
						n), resourcesToBuild.toString());
			}
			if (n > 0) {
				notifier.setProgressPerCompilationUnit(1.0f / n);
				for (IResource resource : resourcesToBuild) {
					// TODO call these in parallel - how to gather markers?
					notifier.aboutToCompile(resource);
					if ("erl".equals(resource.getFileExtension())) {
						compileFile(currentProject, resource);
					} else if ("yrl".equals(resource.getFileExtension())) {
						compileYrlFile(currentProject, resource);
					} else {
						ErlLogger.warn("Don't know how to compile: %s",
								resource.getName());
					}
					notifier.compiled(resource);
				}
			}
			notifier.done();

			try {
				checkForClashes();
			} catch (final Exception e) {
			}

		} catch (final CoreException e) {
			ErlLogger.error(e);
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
		final Backend b = ErlangCore.getBackendManager().getBuildBackend(
				currentProject);
		try {
			final OtpErlangList res = ErlideBuilder.getCodeClashes(b);
			for (int i = 0; i < res.arity(); i++) {
				final OtpErlangTuple t = (OtpErlangTuple) res.elementAt(i);
				final String f1 = ((OtpErlangString) t.elementAt(0))
						.stringValue();
				final String f2 = ((OtpErlangString) t.elementAt(1))
						.stringValue();

				// add marker only for modules belonging to this project!

				final IResource r1 = currentProject.findMember(f1);
				final IResource r2 = currentProject.findMember(f2);
				// XXX does the above work? or do we need to get the name only?
				if (r1 != null || r2 != null) {
					generator.addMarker(currentProject, currentProject,
							"Code clash between " + f1 + " and " + f2, 0,
							IMarker.SEVERITY_WARNING, "");
				}
			}

		} catch (final Exception e) {
		}
		try {
			final ErlangProjectProperties pp = ErlangCore
					.getProjectProperties(currentProject);
			final String[] sd = pp.getSourceDirs();
			final String[] dirList = new String[sd.length];
			for (int i = 0; i < sd.length; i++) {
				dirList[i] = currentProject.getLocation().toPortableString()
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
				generator.addMarker(currentProject, currentProject,
						"Duplicated module name in " + f1 + " and " + f2, 0,
						IMarker.SEVERITY_ERROR, "");
			}
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
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
	protected Set<IResource> fullBuild(final Map args) throws CoreException {
		HashSet<IResource> result = new HashSet<IResource>();
		getProject().accept(new ErlangResourceVisitor(result));
		return result;
	}

	@SuppressWarnings("unchecked")
	protected Set<IResource> incrementalBuild(final Map args,
			final IResourceDelta delta) throws CoreException {
		HashSet<IResource> result = new HashSet<IResource>();
		delta.accept(new ErlangDeltaVisitor(result));
		return result;
	}

	protected void compileFile(final IProject project, final IResource resource) {
		final IPath projectPath = project.getLocation();
		final ErlangProjectProperties prefs = ErlangCore
				.getProjectProperties(currentProject);

		final String s = resource.getFileExtension();
		if (!s.equals("erl")) {
			System.out.println("?!?!");
		}

		deleteMarkers(resource);
		if (isInExtCodePath(resource, project)
				&& !isInCodePath(resource, project)) {
			final String msg = " is not directly on source path (packages are not supported yet)";
			generator.addMarker(resource, resource, resource.getName() + msg,
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
						final Collection<ErlangIncludeFile> incs = m
								.getIncludedFiles();
						for (final ErlangIncludeFile ifile : incs) {
							final IResource rifile = ErlangBuilderMarkerGenerator
									.findResourceByName(project, ifile
											.getFilename());
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
					ErlangBuilderMarkerGenerator.addProblemMarker(resource,
							null, "Could not compile file", 0,
							IMarker.SEVERITY_ERROR);
					return;
				}
				final OtpErlangTuple t = (OtpErlangTuple) r;
				// ErlLogger.debug("** " + r);

				if ("ok".equals(((OtpErlangAtom) t.elementAt(0)).atomValue())) {
					final String beamf = resource.getFullPath()
							.removeFileExtension().lastSegment();
					// final OtpErlangBinary code = (OtpErlangBinary) t
					// .elementAt(2);
					// for (Backend b :
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

				// ErlLogger.debug("t = " + t);
				// process compilation messages
				final OtpErlangList l = (OtpErlangList) t.elementAt(1);
				ErlangBuilderMarkerGenerator.addErrorMarkers(generator,
						resource, l);
			} else {
				if (BuilderUtils.isDebugging()) {
					ErlLogger.debug("skipping %s", resource.getName());
				}
			}
			br = project.findMember(beam);
			if (br != null) {
				br.setDerived(true);
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
		final ErlangProjectProperties prefs = ErlangCore
				.getProjectProperties(project);
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
				final Collection<IErlComment> cl = s.getComments();
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
			ErlangBuilderMarkerGenerator.addTaskMarker(resource, resource, msg,
					c.getLineStart() + 1 + dl, prio);
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
			generator
					.addMarker(
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
				ErlangBuilderMarkerGenerator.addErrorMarkers(generator,
						resource, l);
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
		final ErlangProjectProperties prefs = ErlangCore
				.getProjectProperties(project);
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
		final ErlangProjectProperties prefs = ErlangCore
				.getProjectProperties(project);
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
		final ErlangProjectProperties prefs = ErlangCore
				.getProjectProperties(project);
		final IPath projectPath = project.getLocation();

		final String out = prefs.getOutputDir();
		return projectPath.append(new Path(out)).isPrefixOf(
				resource.getLocation());
	}

	@SuppressWarnings("unused")
	private static void distributeModule(final Backend b, final String beamf,
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

		private final Set<IResource> result;

		public ErlangDeltaVisitor(Set<IResource> result) {
			this.result = result;
		}

		public boolean visit(final IResourceDelta delta) throws CoreException {

			final IResource resource = delta.getResource();
			final IProject my_project = resource.getProject();
			final ErlangProjectProperties prefs = ErlangCore
					.getProjectProperties(my_project);

			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "erl".equals(resource.getFileExtension())
					&& isInExtCodePath(resource, my_project)) {
				switch (delta.getKind()) {
				case IResourceDelta.ADDED:
				case IResourceDelta.CHANGED:
					// handle changed resource
					if (!resource.isDerived()) {
						result.add(resource);
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
						result.add(yrl);
					}

					break;
				}
			}
			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "hrl".equals(resource.getFileExtension())
					&& isInIncludedPath(resource, my_project)) {
				switch (delta.getKind()) {
				case IResourceDelta.ADDED:
					break;
				case IResourceDelta.REMOVED:
				case IResourceDelta.CHANGED:
					addDependents(resource, my_project, result);
					break;
				}
			}
			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "yrl".equals(resource.getFileExtension())
					&& isInExtCodePath(resource, my_project)) {
				switch (delta.getKind()) {
				case IResourceDelta.ADDED:
				case IResourceDelta.CHANGED:
					result.add(resource);
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
					final String[] p = resource.getName().split("\\.");
					final ErlangFileVisitor searcher = new ErlangFileVisitor(
							p[0], null);
					my_project.accept(searcher);
					if (searcher.fResult != null) {
						result.add(searcher.fResult);
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

	void addDependents(final IResource resource, final IProject my_project,
			final Set<IResource> result) throws ErlModelException {
		final IErlProject eprj = ErlangCore.getModel().findProject(my_project);
		if (eprj != null) {
			final List<IErlModule> ms = eprj.getModules();
			for (final IErlModule m : ms) {
				final Collection<ErlangIncludeFile> incs = m.getIncludedFiles();
				for (final ErlangIncludeFile ifile : incs) {
					if (ErlangBuilderMarkerGenerator.comparePath(ifile
							.getFilename(), resource.getName())) {
						if (m.getModuleKind() == ModuleKind.ERL) {
							result.add(m.getResource());
						}
						break;
					}
				}
			}
		}
	}

	class ErlangResourceVisitor implements IResourceVisitor {

		private final Set<IResource> result;

		public ErlangResourceVisitor(Set<IResource> result) {
			this.result = result;
		}

		public boolean visit(final IResource resource) throws CoreException {
			final IProject my_project = resource.getProject();

			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "erl".equals(resource.getFileExtension())
					&& isInExtCodePath(resource, my_project)
					&& !resource.isDerived()) {

				try {
					result.add(resource);
				} catch (final Exception e) {
					e.printStackTrace();
				}
			}
			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "hrl".equals(resource.getFileExtension())
					&& isInIncludedPath(resource, my_project)) {
				addDependents(resource, my_project, result);
			}
			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "yrl".equals(resource.getFileExtension())
					&& isInExtCodePath(resource, my_project)) {

				try {
					result.add(resource);
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

		public ErlangFileVisitor(final String name,
				final IProgressMonitor submon) {
			fResult = null;
			fName = name;
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

	private void cleanup() {
		notifier = null;
	}

	private void initializeBuilder() throws CoreException {
		workspaceRoot = currentProject.getWorkspace().getRoot();
	}

	@SuppressWarnings("unchecked")
	protected IProject[] build_remote(final int kind, final Map args)
			throws CoreException {
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
}
