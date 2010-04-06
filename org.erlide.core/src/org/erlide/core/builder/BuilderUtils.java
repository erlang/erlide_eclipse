/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.builder;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.builder.internal.MarkerHelper;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IErlModule.ModuleKind;
import org.erlide.core.erlang.util.ErlangIncludeFile;
import org.erlide.core.preferences.OldErlangProjectProperties;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.jinterface.rpc.RpcFuture;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Sets;

import erlang.ErlideBuilder;

public final class BuilderUtils {
	private static class ErlangDeltaVisitor implements IResourceDeltaVisitor {

		private final Set<BuildResource> result;
		private final IProgressMonitor monitor;

		public ErlangDeltaVisitor(final Set<BuildResource> result,
				final IProgressMonitor monitor) {
			this.result = result;
			this.monitor = monitor;
		}

		public boolean visit(final IResourceDelta delta) throws CoreException {
			final IResource resource = delta.getResource();
			final IProject my_project = resource.getProject();
			final OldErlangProjectProperties prefs = ErlangCore
					.getProjectProperties(my_project);

			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "erl".equals(resource.getFileExtension())) {
				handleErlFile(delta, resource, my_project, prefs);
			}
			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "hrl".equals(resource.getFileExtension())
					&& isInIncludedPath(resource, my_project)) {
				handleHrlFile(delta, resource, my_project);
			}
			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "yrl".equals(resource.getFileExtension())
					&& isInCodePath(resource, my_project)) {
				handleYrlFile(delta, resource, my_project);
			}
			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "beam".equals(resource.getFileExtension())
					&& isInOutputPath(resource, my_project)) {
				handleBeamFile(delta, resource, my_project);
			}
			// return true to continue visiting children.
			if (resource.getType() == IResource.FOLDER
					|| resource.getType() == IResource.PROJECT) {
				return isInteresting(resource, my_project);
			}
			return true;
		}

		private void handleBeamFile(final IResourceDelta delta,
				final IResource resource, final IProject my_project)
				throws CoreException {
			switch (delta.getKind()) {
			case IResourceDelta.ADDED:
			case IResourceDelta.CHANGED:
				break;
			case IResourceDelta.REMOVED:
				final String[] p = resource.getName().split("\\.");
				final SearchVisitor searcher = new SearchVisitor(p[0], null);
				my_project.accept(searcher);
				if (searcher.fResult != null) {
					// FIXME BuildResource
					final BuildResource bres = new BuildResource(
							searcher.fResult);
					result.add(bres);
					monitor.worked(1);
				}
				break;
			}
		}

		private void handleYrlFile(final IResourceDelta delta,
				final IResource resource, final IProject my_project) {
			switch (delta.getKind()) {
			case IResourceDelta.ADDED:
			case IResourceDelta.CHANGED:
				// FIXME BuildResource
				final BuildResource bres = new BuildResource(resource);
				result.add(bres);
				monitor.worked(1);
				break;

			case IResourceDelta.REMOVED:
				MarkerHelper.deleteMarkers(resource);

				IPath erl = resource.getProjectRelativePath()
						.removeFileExtension();
				erl = erl.addFileExtension("erl").setDevice(null);
				final IResource br = my_project.findMember(erl);
				if (br != null) {
					try {
						br.delete(true, null);
					} catch (final Exception e) {
						ErlLogger.warn(e);
					}
					monitor.worked(1);
				}
				break;
			}
		}

		private void handleHrlFile(final IResourceDelta delta,
				final IResource resource, final IProject my_project)
				throws ErlModelException {
			switch (delta.getKind()) {
			case IResourceDelta.ADDED:
			case IResourceDelta.REMOVED:
			case IResourceDelta.CHANGED:
				final int n = result.size();
				addDependents(resource, my_project, result);
				monitor.worked(result.size() - n);
				break;
			}
		}

		private void handleErlFile(final IResourceDelta delta,
				final IResource resource, final IProject my_project,
				final OldErlangProjectProperties prefs) {
			switch (delta.getKind()) {
			case IResourceDelta.ADDED:
			case IResourceDelta.CHANGED:
				// handle changed resource
				if (!resource.isDerived()) {
					if (isInCodePath(resource, my_project)) {
						final BuildResource bres = new BuildResource(resource);
						result.add(bres);
						monitor.worked(1);
					}
				}
				break;
			case IResourceDelta.REMOVED:
				// handle removed resource
				MarkerHelper.deleteMarkers(resource);

				IPath beam = new Path(prefs.getOutputDir());
				final IPath module = beam.append(resource.getName())
						.removeFileExtension();
				beam = module.addFileExtension("beam").setDevice(null);
				final IResource br = my_project.findMember(beam);
				if (br != null) {
					try {
						br.delete(true, null);
					} catch (final Exception e) {
						ErlLogger.warn(e);
					}
				}

				// was it derived from a yrl?
				final IPath yrlp = resource.getProjectRelativePath()
						.removeFileExtension().addFileExtension("yrl");
				final IResource yrl = my_project.findMember(yrlp);
				if (yrl != null) {
					// FIXME BuildResource
					final BuildResource bres = new BuildResource(resource);
					result.add(bres);
					monitor.worked(1);
				}

				break;
			}
		}
	}

	private static class ErlangResourceVisitor implements IResourceVisitor {

		private final Set<BuildResource> result;
		private final IProgressMonitor monitor;

		public ErlangResourceVisitor(final Set<BuildResource> result,
				final IProgressMonitor monitor) {
			this.result = result;
			this.monitor = monitor;
		}

		public boolean visit(final IResource resource) throws CoreException {
			final IProject my_project = resource.getProject();
			if (resource.isDerived()) {
				return true;
			}
			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "erl".equals(resource.getFileExtension())) {
				try {
					// FIXME BuildResource
					if (isInCodePath(resource, my_project)) {
						final BuildResource bres = new BuildResource(resource);
						result.add(bres);
						monitor.worked(1);
					}

				} catch (final Exception e) {
					e.printStackTrace();
				}
			}
			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "yrl".equals(resource.getFileExtension())
					&& isInCodePath(resource, my_project)) {
				try {
					final BuildResource bres = new BuildResource(resource);
					result.add(bres);
					monitor.worked(1);
				} catch (final Exception e) {
					e.printStackTrace();
				}
			}
			// return true to continue visiting children.
			if (resource.getType() == IResource.FOLDER) {
				return isInteresting(resource, my_project);
			}
			return true;
		}
	}

	private static class SearchVisitor implements IResourceVisitor {

		IResource fResult;
		String fName;

		public SearchVisitor(final String name, final IProgressMonitor monitor) {
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
					&& isInCodePath(resource, resource.getProject())) {
				final String[] p = resource.getName().split("\\.");
				if (p[0].equals(fName)) {
					fResult = resource;
					return false;
				}
			}
			return true;
		}
	}

	private BuilderUtils() {
	}

	public static boolean isDebugging() {
		if (ErlangPlugin.getDefault() == null) {
			return false;
		}
		return ErlangPlugin.getDefault().isDebugging()
				&& Platform.getDebugOption("org.erlide.core/debug/builder")
						.equals("true");
	}

	public static List<String> getAllIncludeDirs(final IProject project) {
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
		return includeDirs;
	}

	/**
	 * @param project
	 * @param prefs
	 * @return
	 */
	public static List<String> getIncludeDirs(final IProject project,
			final List<String> includeDirs) {
		final OldErlangProjectProperties prefs = ErlangCore
				.getProjectProperties(project);
		final List<String> incs = prefs.getIncludeDirs();
		final IPathVariableManager pvm = ResourcesPlugin.getWorkspace()
				.getPathVariableManager();
		for (final String s : incs) {
			final IPath inc = pvm.resolvePath(new Path(s));
			if (inc.isAbsolute()) {
				includeDirs.add(inc.toString());
			} else {
				final IFolder folder = project.getFolder(s);
				if (folder != null) {
					final IPath location = folder.getLocation();
					includeDirs.add(location.toString());
				}
			}
		}
		return includeDirs;
	}

	public static boolean isInteresting(final IResource resource,
			final IProject project) {
		final OldErlangProjectProperties prefs = ErlangCore
				.getProjectProperties(project);

		final List<String> interestingPaths = new ArrayList<String>();
		for (final String s : prefs.getSourceDirs()) {
			interestingPaths.add(s);
		}
		for (final String s : prefs.getIncludeDirs()) {
			interestingPaths.add(s);
		}
		interestingPaths.add(prefs.getOutputDir());

		final IPath projectPath = project.getFullPath();
		final IPath fullPath = resource.getFullPath();
		for (final String element : interestingPaths) {
			final IPath sp = projectPath.append(new Path(element));
			if (fullPath.isPrefixOf(sp)) {
				return true;
			}
		}
		return false;
	}

	public static boolean isInCodePath(final IResource resource,
			final IProject project) {
		final OldErlangProjectProperties prefs = ErlangCore
				.getProjectProperties(project);
		final IPath projectPath = project.getFullPath();
		final List<String> srcs = prefs.getSourceDirs();
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

	public static boolean isInIncludedPath(final IResource resource,
			final IProject my_project) {
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

	public static boolean isInOutputPath(final IResource resource,
			final IProject project) {
		final OldErlangProjectProperties prefs = ErlangCore
				.getProjectProperties(project);
		final IPath projectPath = project.getLocation();

		final String out = prefs.getOutputDir();
		return projectPath.append(new Path(out)).isPrefixOf(
				resource.getLocation());
	}

	static void addDependents(final IResource resource,
			final IProject my_project, final Set<BuildResource> result)
			throws ErlModelException {
		final IErlProject eprj = ErlangCore.getModel().findProject(my_project);
		if (eprj != null) {
			final List<IErlModule> ms = eprj.getModules();
			for (final IErlModule m : ms) {
				final boolean wasKnown = m.isStructureKnown();
				final Collection<ErlangIncludeFile> incs = m.getIncludedFiles();
				for (final ErlangIncludeFile ifile : incs) {
					if (BuilderUtils.samePath(ifile.getFilename(), resource
							.getName())) {
						if (m.getModuleKind() == ModuleKind.ERL) {
							// FIXME BuildResource
							final BuildResource bres = new BuildResource(m
									.getResource());
							result.add(bres);
						}
						break;
					}
				}
				if (!wasKnown) {
					m.disposeScanner();
					m.disposeParser();
				}
			}
		}
	}

	@SuppressWarnings("unchecked")
	public static Set<BuildResource> getAffectedResources(final Map args,
			final IProject project, final IProgressMonitor monitor)
			throws CoreException {
		final Set<BuildResource> result = Sets.newHashSet();
		project.accept(new ErlangResourceVisitor(result, monitor));
		return result;
	}

	@SuppressWarnings("unchecked")
	public static Set<BuildResource> getAffectedResources(final Map args,
			final IResourceDelta delta, final IProgressMonitor monitor)
			throws CoreException {
		final Set<BuildResource> result = Sets.newHashSet();
		if (delta != null) {
			delta.accept(new ErlangDeltaVisitor(result, monitor));
		}
		return result;
	}

	public static void checkForClashes(final Backend backend,
			final IProject project) {
		try {
			final OtpErlangList res = ErlideBuilder.getCodeClashes(backend);
			for (final OtpErlangObject elem : res.elements()) {
				final OtpErlangTuple t = (OtpErlangTuple) elem;
				final String f1 = ((OtpErlangString) t.elementAt(0))
						.stringValue();
				final String f2 = ((OtpErlangString) t.elementAt(1))
						.stringValue();

				// add marker only for modules belonging to this project!

				final IResource r1 = project.findMember(f1);
				final IResource r2 = project.findMember(f2);
				// XXX does the above work? or do we need to get the name only?
				if (r1 != null || r2 != null) {
					MarkerHelper.addMarker(project, project,
							"Code clash between " + f1 + " and " + f2, 0,
							IMarker.SEVERITY_WARNING, "");
				}
			}

		} catch (final Exception e) {
		}
		try {
			final OldErlangProjectProperties pp = ErlangCore
					.getProjectProperties(project);
			final List<String> sd = pp.getSourceDirs();
			final String[] dirList = new String[sd.size()];
			for (int i = 0; i < sd.size(); i++) {
				dirList[i] = project.getLocation().toPortableString() + "/"
						+ sd.get(i);
			}
			final OtpErlangList res = ErlideBuilder.getSourceClashes(backend,
					dirList);
			for (int i = 0; i < res.arity(); i++) {
				final OtpErlangTuple t = (OtpErlangTuple) res.elementAt(i);
				final String f1 = ((OtpErlangString) t.elementAt(0))
						.stringValue();
				final String f2 = ((OtpErlangString) t.elementAt(1))
						.stringValue();
				MarkerHelper.addMarker(project, project,
						"Duplicated module name in " + f1 + " and " + f2, 0,
						IMarker.SEVERITY_ERROR, "");
			}
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

	public static void ensureDirExists(final String outputDir) {
		final File f = new File(outputDir);
		f.mkdir();
	}

	public static String buildKind(final int kind) {
		switch (kind) {
		case IncrementalProjectBuilder.AUTO_BUILD:
			return "auto";
		case IncrementalProjectBuilder.CLEAN_BUILD:
			return "clean";
		case IncrementalProjectBuilder.FULL_BUILD:
			return "full";
		case IncrementalProjectBuilder.INCREMENTAL_BUILD:
			return "incremental";
		default:
			return "unknown";
		}
	}

	public static boolean shouldCompile(final IProject project,
			final IResource source, final IResource beam)
			throws ErlModelException {
		boolean shouldCompile = beam == null;

		if (beam != null) {
			final IErlProject eprj = ErlangCore.getModel().findProject(project);
			if (eprj != null) {
				final IErlModule m = eprj.getModule(source.getName());
				if (m != null) {
					final boolean wasKnown = m.isStructureKnown();
					final Collection<ErlangIncludeFile> incs = m
							.getIncludedFiles();
					for (final ErlangIncludeFile ifile : incs) {
						final IResource rifile = BuilderUtils
								.findResourceByName(project, ifile
										.getFilename());
						if (rifile != null
								&& rifile.getLocalTimeStamp() > beam
										.getLocalTimeStamp()) {
							shouldCompile = true;
							break;
						}
					}
					if (!wasKnown) {
						m.disposeScanner();
						m.disposeParser();
					}
				}
			}
		}

		if (beam != null) {
			shouldCompile |= beam.getLocalTimeStamp() < source
					.getLocalTimeStamp();
		}
		return shouldCompile;
	}

	// public static IResource findResourceByName(final IContainer container,
	// final String fileName) {
	// try {
	// for (final IResource r : container.members()) {
	// if (samePath(r.getName(), fileName)) {
	// return r;
	// }
	// if (r instanceof IContainer) {
	// final IResource res = findResourceByName((IContainer) r,
	// fileName);
	// if (res != null) {
	// return res;
	// }
	// }
	// }
	// } catch (final CoreException e) {
	// e.printStackTrace();
	// }
	// return null;
	// }

	public static boolean samePath(final String p1, final String p2) {
		final boolean WINDOWS = java.io.File.separatorChar == '\\';
		if (WINDOWS) {
			return p1.equalsIgnoreCase(p2);
		} else {
			return p1.equals(p2);
		}
	}

	private static final class FindResourceVisitor implements IResourceVisitor {
		private static final int FIND_BY_NAME = 1;
		private static final int FIND_BY_LOCATION = 2;

		private final String fileName;
		private IResource found = null;
		private final int how;

		private FindResourceVisitor(final String fileName, final int how) {
			this.fileName = fileName;
			this.how = how;
		}

		public boolean visit(final IResource resource) throws CoreException {
			if (compare(resource, fileName, how)) {
				found = resource;
				return false;
			}
			return true;
		}

		private boolean compare(final IResource resource, final String s,
				final int how) {
			if (how == FIND_BY_NAME) {
				return samePath(resource.getName(), s);
			} else if (how == FIND_BY_LOCATION) {
				return samePath(resource.getLocation().toString(), s);
			} else {
				return false;
			}
		}

		public IResource getFound() {
			return found;
		}
	}

	public static IResource findResourceByLocation(final IContainer container,
			final String fileName) {
		return findResource(container, fileName,
				FindResourceVisitor.FIND_BY_LOCATION);
	}

	public static IResource findResourceByName(final IContainer container,
			final String fileName) {
		return findResource(container, fileName,
				FindResourceVisitor.FIND_BY_NAME);
	}

	private static IResource findResource(final IContainer container,
			final String fileName, final int how) {
		final FindResourceVisitor visitor = new FindResourceVisitor(fileName,
				how);
		try {
			container.accept(visitor);
		} catch (final CoreException e) {
			return null;
		}
		return visitor.getFound();
	}

	public static void refreshOutputDir(final IProject project)
			throws CoreException {
		final OldErlangProjectProperties prefs = ErlangCore
				.getProjectProperties(project);
		final String outputDir = prefs.getOutputDir();
		final IResource ebinDir = project.findMember(outputDir);
		if (ebinDir != null) {
			ebinDir.refreshLocal(IResource.DEPTH_ONE, null);
		}
	}

	public static void completeCompile(final IProject project,
			final IResource source, final OtpErlangObject r,
			final Backend backend, final OtpErlangList compilerOptions) {
		if (r == null) {
			MarkerHelper.addProblemMarker(source, null,
					"Could not compile file", 0, IMarker.SEVERITY_ERROR);
			return;
		}
		final OtpErlangTuple t = (OtpErlangTuple) r;
		// ErlLogger.debug("** " + r);

		if ("ok".equals(((OtpErlangAtom) t.elementAt(0)).atomValue())) {
			final String beamf = source.getFullPath().removeFileExtension()
					.lastSegment();
			ErlideBuilder.loadModule(project, beamf);
		} else {
			// ErlLogger.debug(">>>> compile error... %s\n   %s",
			// resource.getName(), t);
		}

		// process compilation messages
		if (t.elementAt(1) instanceof OtpErlangList) {
			final OtpErlangList l = (OtpErlangList) t.elementAt(1);
			MarkerHelper.addErrorMarkers(source, l);
		} else {
			ErlLogger.warn("bad result from builder: %s", t);
		}

		// TODO separate

		// YRL
		final IPath erl = getErlForYrl(source);
		if (erl != null) {
			try {
				source.getParent().refreshLocal(IResource.DEPTH_ONE, null);
				final IResource br = project.findMember(erl);
				if (br != null) {
					br.setDerived(true);
					final BuildResource bbr = new BuildResource(br);
					// br.touch() doesn't work...
					compileErl(project, bbr, backend, compilerOptions);
				}
			} catch (final CoreException e) {
				ErlLogger.warn(e);
			}
		}

	}

	public static RpcFuture startCompileErl(final IProject project,
			final BuildResource bres, final Backend backend,
			final OtpErlangList compilerOptions, final boolean force) {
		final IPath projectPath = project.getLocation();
		final OldErlangProjectProperties prefs = ErlangCore
				.getProjectProperties(project);
		final IResource res = bres.getResource();
		final String s = res.getFileExtension();
		if (!"erl".equals(s)) {
			ErlLogger.warn("trying to compile " + res.getName() + "?!?!");
		}

		MarkerHelper.deleteMarkers(res);

		final String outputDir = bres.getOutput() == null ? projectPath.append(
				prefs.getOutputDir()).toString() : (bres.getOutput()
				.startsWith("/") ? bres.getOutput() : projectPath.append(
				bres.getOutput()).toString());
		ensureDirExists(outputDir);

		final List<String> includeDirs = getAllIncludeDirs(project);

		// delete beam file
		final IPath beamPath = getBeamForErl(res);
		final IResource beam = project.findMember(beamPath);

		try {
			final boolean shouldCompile = force
					|| shouldCompile(project, res, beam);

			if (shouldCompile) {
				if (beam != null) {
					try {
						beam.delete(true, null);
					} catch (final Exception e) {
						ErlLogger.warn(e);
					}
				}
				if (isDebugging()) {
					ErlLogger.debug("compiling %s", res.getName());
				}

				MarkerHelper.createTaskMarkers(project, res);

				return ErlideBuilder.compileErl(backend, res.getLocation()
						.toString(), outputDir, includeDirs, compilerOptions);
			} else {
				return null;
			}
		} catch (final Exception e) {
			ErlLogger.warn(e);
			return null;
		}
	}

	private static IPath getBeamForErl(final IResource source) {
		final OldErlangProjectProperties prefs = ErlangCore
				.getProjectProperties(source.getProject());
		IPath p = new Path(prefs.getOutputDir());
		p = p.append(source.getName());
		if (!"erl".equals(p.getFileExtension())) {
			return null;
		}
		final IPath module = p.removeFileExtension();
		final IPath beamPath = module.addFileExtension("beam").setDevice(null);
		return beamPath;
	}

	public static RpcFuture startCompileYrl(final IProject project,
			final IResource resource, final Backend backend,
			final OtpErlangList compilerOptions) {
		// final IPath projectPath = project.getLocation();
		// final OldErlangProjectProperties prefs = new
		// OldErlangProjectProperties(project);

		MarkerHelper.deleteMarkers(resource);
		// try {
		// resource.deleteMarkers(PROBLEM_MARKER, true,
		// IResource.DEPTH_INFINITE);
		// } catch (final CoreException e1) {
		// }

		final IPath erl = getErlForYrl(resource);
		final IResource br = project.findMember(erl);

		// TODO check timestamps!

		try {
			if (br != null) {
				try {
					br.delete(true, null);
				} catch (final Exception e) {
					ErlLogger.warn(e);
				}
			}

			final String input = resource.getLocation().toString();
			final String output = resource.getLocation().removeFileExtension()
					.toString();
			return ErlideBuilder.compileYrl(backend, input, output);
		} catch (final Exception e) {
			e.printStackTrace();
			return null;
		}

	}

	private static IPath getErlForYrl(final IResource resource) {
		final IPath path = resource.getProjectRelativePath();
		if (!"yrl".equals(path.getFileExtension())) {
			return null;
		}
		IPath erl = path.removeFileExtension();
		erl = erl.addFileExtension("erl").setDevice(null);
		return erl;
	}

	public static void compileErl(final IProject project,
			final BuildResource resource, final Backend b,
			final OtpErlangList compilerOptions) {
		final RpcFuture res = startCompileErl(project, resource, b,
				compilerOptions, true);
		if (res == null) {
			ErlLogger.warn("error compiling erl file: "
					+ resource.getResource().getProjectRelativePath());
		}
		try {
			completeCompile(project, resource.getResource(), res.get(), b,
					compilerOptions);
		} catch (final RpcException e) {
			ErlLogger.warn(e);
		}
	}

	public static void compileYrl(final IProject project,
			final BuildResource resource, final Backend b,
			final OtpErlangList compilerOptions) {
		final RpcFuture res = startCompileYrl(project, resource.getResource(),
				b, compilerOptions);
		if (res == null) {
			ErlLogger.warn("error compiling yrl file: "
					+ resource.getResource().getProjectRelativePath());
		}
		try {
			completeCompile(project, resource.getResource(), res.get(), b,
					compilerOptions);
		} catch (final RpcException e) {
			ErlLogger.warn(e);
		}
	}

}
