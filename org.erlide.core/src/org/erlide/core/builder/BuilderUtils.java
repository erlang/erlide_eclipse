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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

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
import org.erlide.core.builder.internal.MarkerGenerator;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IErlModule.ModuleKind;
import org.erlide.core.erlang.util.ErlangIncludeFile;
import org.erlide.core.preferences.OldErlangProjectProperties;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideBuilder;

public final class BuilderUtils {
	private static class ErlangDeltaVisitor implements IResourceDeltaVisitor {

		private final Set<IResource> result;
		private final IProgressMonitor monitor;

		public ErlangDeltaVisitor(final Set<IResource> result,
				IProgressMonitor monitor) {
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
					&& "erl".equals(resource.getFileExtension())
					&& isInCodePath(resource, my_project)) {
				switch (delta.getKind()) {
				case IResourceDelta.ADDED:
				case IResourceDelta.CHANGED:
					// handle changed resource
					if (!resource.isDerived()) {
						result.add(resource);
						monitor.worked(1);
					}
					break;
				case IResourceDelta.REMOVED:
					// handle removed resource
					MarkerGenerator.deleteMarkers(resource);

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
						monitor.worked(1);
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
				case IResourceDelta.REMOVED:
				case IResourceDelta.CHANGED:
					int n = result.size();
					addDependents(resource, my_project, result);
					monitor.worked(result.size() - n);
					break;
				}
			}
			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "yrl".equals(resource.getFileExtension())
					&& isInCodePath(resource, my_project)) {
				switch (delta.getKind()) {
				case IResourceDelta.ADDED:
				case IResourceDelta.CHANGED:
					result.add(resource);
					monitor.worked(1);
					break;

				case IResourceDelta.REMOVED:
					MarkerGenerator.deleteMarkers(resource);

					IPath erl = resource.getProjectRelativePath()
							.removeFileExtension();
					erl = erl.addFileExtension("erl").setDevice(null);
					final IResource br = my_project.findMember(erl);
					if (br != null) {
						br.delete(true, null);
						monitor.worked(1);
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
					final BuilderUtils.SearchVisitor searcher = new BuilderUtils.SearchVisitor(
							p[0], null);
					my_project.accept(searcher);
					if (searcher.fResult != null) {
						result.add(searcher.fResult);
						monitor.worked(1);
					}
					break;
				}
			}
			// return true to continue visiting children.
			if (resource.getType() == IResource.FOLDER) {
				return isInteresting(resource, my_project);
			}
			return true;
		}
	}

	private static class ErlangResourceVisitor implements IResourceVisitor {

		private final Set<IResource> result;
		private final IProgressMonitor monitor;

		public ErlangResourceVisitor(final Set<IResource> result,
				IProgressMonitor monitor) {
			this.result = result;
			this.monitor = monitor;
		}

		public boolean visit(final IResource resource) throws CoreException {
			final IProject my_project = resource.getProject();

			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "erl".equals(resource.getFileExtension())
					&& isInCodePath(resource, my_project)
					&& !resource.isDerived()) {

				try {
					result.add(resource);
					monitor.worked(1);
				} catch (final Exception e) {
					e.printStackTrace();
				}
			}
			// not needed, we are doing a full build anyway!
			// if (resource.getType() == IResource.FILE
			// && resource.getFileExtension() != null
			// && "hrl".equals(resource.getFileExtension())
			// && isInIncludedPath(resource, my_project)) {
			// int n = result.size();
			// addDependents(resource, my_project, result);
			// monitor.worked(result.size() - n);
			// }
			if (resource.getType() == IResource.FILE
					&& resource.getFileExtension() != null
					&& "yrl".equals(resource.getFileExtension())
					&& isInCodePath(resource, my_project)) {

				try {
					result.add(resource);
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
		private final IProgressMonitor monitor;

		public SearchVisitor(final String name, final IProgressMonitor monitor) {
			fResult = null;
			fName = name;
			this.monitor = monitor;
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

	public static boolean isInteresting(final IResource resource,
			final IProject project) {
		final OldErlangProjectProperties prefs = ErlangCore
				.getProjectProperties(project);

		List<String> interestingPaths = new ArrayList<String>();
		final String[] srcs = prefs.getSourceDirs();
		for (String s : srcs) {
			interestingPaths.add(s);
		}
		String[] incs = prefs.getIncludeDirs();
		for (String s : incs) {
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

	private static void addDependents(final IResource resource,
			final IProject my_project, final Set<IResource> result)
			throws ErlModelException {
		final IErlProject eprj = ErlangCore.getModel().findProject(my_project);
		if (eprj != null) {
			final List<IErlModule> ms = eprj.getModules();
			for (final IErlModule m : ms) {
				final Collection<ErlangIncludeFile> incs = m.getIncludedFiles();
				for (final ErlangIncludeFile ifile : incs) {
					if (MarkerGenerator.comparePath(ifile.getFilename(),
							resource.getName())) {
						if (m.getModuleKind() == ModuleKind.ERL) {
							result.add(m.getResource());
						}
						break;
					}
				}
			}
		}
	}

	@SuppressWarnings("unchecked")
	public static Set<IResource> getAffectedResources(final Map args,
			final IProject project, IProgressMonitor monitor)
			throws CoreException {
		final HashSet<IResource> result = new HashSet<IResource>();
		project.accept(new BuilderUtils.ErlangResourceVisitor(result, monitor));
		return result;
	}

	@SuppressWarnings("unchecked")
	public static Set<IResource> getAffectedResources(final Map args,
			final IResourceDelta delta, IProgressMonitor monitor)
			throws CoreException {
		final HashSet<IResource> result = new HashSet<IResource>();
		if (delta != null) {
			delta.accept(new BuilderUtils.ErlangDeltaVisitor(result, monitor));
		}
		return result;
	}

	public static void checkForClashes(final Backend backend, IProject project) {
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
					MarkerGenerator.addMarker(project, project,
							"Code clash between " + f1 + " and " + f2, 0,
							IMarker.SEVERITY_WARNING, "");
				}
			}

		} catch (final Exception e) {
		}
		try {
			final OldErlangProjectProperties pp = ErlangCore
					.getProjectProperties(project);
			final String[] sd = pp.getSourceDirs();
			final String[] dirList = new String[sd.length];
			for (int i = 0; i < sd.length; i++) {
				dirList[i] = project.getLocation().toPortableString() + "/"
						+ sd[i];
			}
			final OtpErlangList res = ErlideBuilder.getSourceClashes(backend,
					dirList);
			for (int i = 0; i < res.arity(); i++) {
				final OtpErlangTuple t = (OtpErlangTuple) res.elementAt(i);
				final String f1 = ((OtpErlangString) t.elementAt(0))
						.stringValue();
				final String f2 = ((OtpErlangString) t.elementAt(1))
						.stringValue();
				MarkerGenerator.addMarker(project, project,
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

	public static String buildKind(int kind) {
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

	public static void compileFile(final IProject project,
			final IResource resource, final Backend backend,
			OtpErlangList compilerOptions) {
		final IPath projectPath = project.getLocation();
		final OldErlangProjectProperties prefs = ErlangCore
				.getProjectProperties(project);

		final String s = resource.getFileExtension();
		if (!s.equals("erl")) {
			ErlLogger.warn("trying to compile " + resource.getName() + "?!?!");
		}

		MarkerGenerator.deleteMarkers(resource);

		final String outputDir = projectPath.append(prefs.getOutputDir())
				.toString();
		ensureDirExists(outputDir);

		List<String> includeDirs = getAllIncludeDirs(project);

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
							final IResource rifile = MarkerGenerator
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
				if (isDebugging()) {
					ErlLogger.debug("compiling %s", resource.getName());
				}

				MarkerGenerator.createTaskMarkers(project, resource);

				OtpErlangObject r;
				r = compileFile(backend, resource.getLocation().toString(),
						outputDir, includeDirs, compilerOptions);
				if (r == null) {
					MarkerGenerator
							.addProblemMarker(resource, null,
									"Could not compile file", 0,
									IMarker.SEVERITY_ERROR);
					return;
				}
				final OtpErlangTuple t = (OtpErlangTuple) r;
				// ErlLogger.debug("** " + r);

				if ("ok".equals(((OtpErlangAtom) t.elementAt(0)).atomValue())) {
					final String beamf = resource.getFullPath()
							.removeFileExtension().lastSegment();
					ErlideBuilder.loadModule(project, beamf);
				} else {
					// ErlLogger.debug(">>>> compile error... %s\n   %s",
					// resource.getName(), t);
				}
				if (br != null) {
					br.getParent().refreshLocal(IResource.DEPTH_ONE, null);
				}
				br = project.findMember(new Path(prefs.getOutputDir()));
				if (br != null) {
					br.refreshLocal(IResource.DEPTH_ONE, null);
				}

				// process compilation messages
				if (t.elementAt(1) instanceof OtpErlangList) {
					final OtpErlangList l = (OtpErlangList) t.elementAt(1);
					MarkerGenerator.addErrorMarkers(resource, l);
				} else {
					ErlLogger.warn("bad result from builder: %s", t);
				}

			} else {
				if (isDebugging()) {
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
	 * Compile Erlang module, given output directory and include directory
	 * 
	 * @throws ErlangRpcException
	 * @param fn
	 * @param outputdir
	 * @param includedirs
	 * @return OtpErlangObject
	 */
	public static OtpErlangObject compileFile(final Backend backend,
			final String fn, final String outputdir,
			final List<String> includedirs, OtpErlangList compilerOptions) {
		if (isDebugging()) {
			ErlLogger.debug("!!! compiling " + fn);
		}
		return ErlideBuilder.compileErl(backend, fn, outputdir, includedirs,
				compilerOptions);
	}

	public static OtpErlangObject compileYrlFile(final Backend backend,
			final String fn, final String output) {
		if (isDebugging()) {
			ErlLogger.debug("!!! compiling " + fn);
		}
		return ErlideBuilder.compileYrl(backend, fn, output);
	}

	public static void compileYrlFile(final IProject project,
			final IResource resource, final Backend backend,
			OtpErlangList compilerOptions) {
		// final IPath projectPath = project.getLocation();
		// final OldErlangProjectProperties prefs = new
		// OldErlangProjectProperties(project);

		MarkerGenerator.deleteMarkers(resource);
		// try {
		// resource.deleteMarkers(PROBLEM_MARKER, true,
		// IResource.DEPTH_INFINITE);
		// } catch (final CoreException e1) {
		// }

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
			r = compileYrlFile(backend, input, output);

			if (r instanceof OtpErlangTuple) {
				// process compilation messages
				final OtpErlangTuple t = (OtpErlangTuple) r;
				final OtpErlangList l = (OtpErlangList) t.elementAt(1);
				MarkerGenerator.addErrorMarkers(resource, l);
			}

			resource.getParent().refreshLocal(IResource.DEPTH_ONE, null);
			br = project.findMember(erl);
			if (br != null) {
				br.setDerived(true);
				// br.touch() doesn't work...
				compileFile(project, br, backend, compilerOptions);
			}

		} catch (final Exception e) {
			e.printStackTrace();
		}

	}

}
