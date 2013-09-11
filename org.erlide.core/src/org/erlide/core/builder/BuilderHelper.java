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
import org.erlide.backend.BackendCore;
import org.erlide.backend.api.IBackend;
import org.erlide.backend.api.IBackendManager;
import org.erlide.core.ErlangPlugin;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.ModuleKind;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.util.ErlangIncludeFile;
import org.erlide.engine.util.ResourceUtil;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.rpc.IRpcFuture;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

public final class BuilderHelper {

    private static final String ERL = "erl";
    private static final String ERLIDE_BUILDER = "erlide_builder";

    public BuilderHelper() {
    }

    public static boolean isDebugging() {
        return ErlangPlugin.getDefault().isDebugging()
                && "true".equalsIgnoreCase(Platform
                        .getDebugOption("org.erlide.core/debug/builder"));
    }

    public Collection<IPath> getAllIncludeDirs(final IProject project) {
        Collection<IPath> includeDirs = getIncludeDirs(project,
                new ArrayList<IPath>());

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

    public Collection<IPath> getIncludeDirs(final IProject project,
            final Collection<IPath> includeDirs) {
        final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                .getErlangProject(project);
        if (erlProject == null) {
            return includeDirs;
        }
        final Collection<IPath> projectIncludeDirs = erlProject
                .getIncludeDirs();
        final IPathVariableManager pvm = ResourcesPlugin.getWorkspace()
                .getPathVariableManager();
        for (IPath inc : projectIncludeDirs) {
            inc = pvm.resolvePath(inc);
            if (inc.isAbsolute()) {
                includeDirs.add(inc);
            } else {
                final IFolder folder = project.getFolder(inc);
                if (folder != null) {
                    final IPath location = folder.getLocation();
                    if (location != null) {
                        includeDirs.add(location);
                    } else {
                        ErlLogger.warn("No location for %s", folder);
                    }
                }
            }
        }
        return includeDirs;
    }

    boolean isInCodePath(final IResource resource, final IErlProject erlProject) {
        final IPath projectPath = resource.getProject().getFullPath();
        final Collection<IPath> srcs = erlProject.getSourceDirs();
        final IPath exceptLastSegment = resource.getFullPath()
                .removeLastSegments(1);
        for (final IPath element : srcs) {
            final IPath sp = projectPath.append(element);
            if (sp.equals(exceptLastSegment)) {
                return true;
            }
        }

        return false;
    }

    public void addDependents(final IResource resource,
            final IProject my_project, final Set<BuildResource> result)
            throws ErlModelException {
        final IErlProject eprj = ErlangEngine.getInstance().getModel()
                .findProject(my_project);
        if (eprj != null) {
            final Collection<IErlModule> ms = eprj.getModules();
            for (final IErlModule m : ms) {
                final Collection<ErlangIncludeFile> incs = m.getIncludeFiles();
                for (final ErlangIncludeFile ifile : incs) {
                    if (ResourceUtil.samePath(ifile.getFilename(),
                            resource.getName())) {
                        if (m.getModuleKind() == ModuleKind.ERL) {
                            final BuildResource bres = new BuildResource(
                                    m.getResource());
                            result.add(bres);
                        }
                        break;
                    }
                }
            }
        }
    }

    public Set<BuildResource> getAffectedResources(
            @SuppressWarnings("rawtypes") final Map args,
            final IProject project, final IProgressMonitor monitor)
            throws CoreException {
        final Set<BuildResource> result = Sets.newHashSet();
        project.accept(new BuilderVisitor(result, monitor, this));
        return result;
    }

    public Set<BuildResource> getAffectedResources(
            @SuppressWarnings("rawtypes") final Map args,
            final IResourceDelta delta, final IProgressMonitor monitor)
            throws CoreException {
        final Set<BuildResource> result = Sets.newHashSet();
        if (delta != null) {
            delta.accept(new BuilderVisitor(result, monitor, this));
        }
        return result;
    }

    public void checkForClashes(final IRpcSite backend, final IProject project) {
        try {
            final OtpErlangList res = BuilderHelper.getCodeClashes(backend);
            for (final OtpErlangObject elem : res) {
                final OtpErlangTuple t = (OtpErlangTuple) elem;
                final String f1 = ((OtpErlangString) t.elementAt(0))
                        .stringValue();
                final String f2 = ((OtpErlangString) t.elementAt(1))
                        .stringValue();

                // add marker only for modules belonging to this project!
                final IResource r1 = project.findMember(f1);
                final IResource r2 = project.findMember(f2);
                if (r1 != null || r2 != null) {
                    MarkerUtils.addMarker(project, null, project,
                            "Code clash between " + f1 + " and " + f2, 0,
                            IMarker.SEVERITY_WARNING, "");
                }
            }

        } catch (final Exception e) {
        }
        try {
            final IErlProject erlProject = ErlangEngine.getInstance()
                    .getModel().getErlangProject(project);
            final Collection<IPath> sd = erlProject.getSourceDirs();
            final String[] dirList = new String[sd.size()];
            int j = 0;
            for (final IPath sp : sd) {
                dirList[j++] = project.getLocation().toPortableString() + "/"
                        + sp;
            }
            final OtpErlangList res = getSourceClashes(backend, dirList);
            for (int i = 0; i < res.arity(); i++) {
                final OtpErlangTuple t = (OtpErlangTuple) res.elementAt(i);
                final String f1 = ((OtpErlangString) t.elementAt(0))
                        .stringValue();
                final String f2 = ((OtpErlangString) t.elementAt(1))
                        .stringValue();
                MarkerUtils.addMarker(project, null, project,
                        "Duplicated module name in " + f1 + " and " + f2, 0,
                        IMarker.SEVERITY_WARNING, "");
            }
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    public void ensureDirExists(final String outputDir) {
        final File f = new File(outputDir);
        f.mkdir();
    }

    public String buildKind(final int kind) {
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

    public boolean shouldCompile(final IProject project,
            final IResource source, final IResource beam)
            throws ErlModelException {
        boolean shouldCompile = beam == null;

        if (beam != null) {
            final IErlProject eprj = ErlangEngine.getInstance().getModel()
                    .findProject(project);
            if (eprj != null) {
                shouldCompile = shouldCompileModule(project, source, beam,
                        shouldCompile, eprj);
            }
        }

        if (beam != null) {
            shouldCompile |= beam.getLocalTimeStamp() < source
                    .getLocalTimeStamp();
        }
        return shouldCompile;
    }

    private boolean shouldCompileModule(final IProject project,
            final IResource source, final IResource beam,
            final boolean shouldCompile0, final IErlProject eprj)
            throws ErlModelException {
        boolean shouldCompile = shouldCompile0;
        final IErlModule m = eprj.getModule(source.getName());
        if (m != null) {
            final Collection<ErlangIncludeFile> incs = m.getIncludeFiles();
            for (final ErlangIncludeFile ifile : incs) {
                final IResource rifile = ResourceUtil.findResourceByName(
                        project, ifile.getFilename());
                if (rifile != null
                        && rifile.getLocalTimeStamp() > beam
                                .getLocalTimeStamp()) {
                    shouldCompile = true;
                    break;
                }
            }
        }
        return shouldCompile;
    }

    public void refreshOutputDir(final IProject project) throws CoreException {
        final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                .getErlangProject(project);
        final IPath outputDir = erlProject.getOutputLocation();
        final IResource ebinDir = project.findMember(outputDir);
        if (ebinDir != null) {
            ebinDir.refreshLocal(IResource.DEPTH_ONE, null);
        }
    }

    public void completeCompile(final IProject project, final IResource source,
            final OtpErlangObject compilationResult, final IRpcSite backend,
            final OtpErlangList compilerOptions) {
        if (compilationResult == null) {
            MarkerUtils.addProblemMarker(source, null, null,
                    "Could not compile file", 0, IMarker.SEVERITY_ERROR);
            return;
        }
        final OtpErlangTuple t = (OtpErlangTuple) compilationResult;
        // ErlLogger.debug("** " + t);

        if ("ok".equals(((OtpErlangAtom) t.elementAt(0)).atomValue())) {
            final String beamf = source.getFullPath().removeFileExtension()
                    .lastSegment();
            BuilderHelper.loadModule(project, beamf);
            refreshDirs(project, t.elementAt(2));
        } else {
            // ErlLogger.debug(">>>> compile error... %s\n   %s",
            // resource.getName(), t);
        }

        // process compilation messages
        if (t.elementAt(1) instanceof OtpErlangList) {
            final OtpErlangList l = (OtpErlangList) t.elementAt(1);
            MarkerUtils.addErrorMarkers(source, l);
        } else {
            ErlLogger.warn("bad result from builder: %s", t);
        }

        completeCompileForYrl(project, source, backend, compilerOptions);
    }

    private void refreshDirs(final IProject project,
            final OtpErlangObject element) {
        final OtpErlangList list = (OtpErlangList) element;
        for (final OtpErlangObject ebeam : list) {
            final OtpErlangString beam = (OtpErlangString) ebeam;
            IPath p = new Path(beam.stringValue());
            p = p.removeLastSegments(1);
            p = p.removeFirstSegments(project.getLocation().segmentCount());
            final String projectName = project.getName();
            // FIXME hardcoded "_erl" suffix
            if (projectName.endsWith("_erl")) {
                final String linkname = projectName.substring(0,
                        projectName.length() - 4);
                p = new Path(linkname).append(p);
            }
            final IResource dir = project.findMember(p);
            if (dir != null) {
                try {
                    dir.refreshLocal(IResource.DEPTH_ONE, null);
                } catch (final CoreException e) {
                }
            }
        }
    }

    private void completeCompileForYrl(final IProject project,
            final IResource source, final IRpcSite backend,
            final OtpErlangList compilerOptions) {
        final IPath erl = getErlForYrl(source);
        if (erl != null) {
            try {
                source.getParent().refreshLocal(IResource.DEPTH_ONE, null);
                final IResource br = project.findMember(erl);
                if (br != null) {
                    br.setDerived(true, null);
                    final BuildResource bbr = new BuildResource(br);
                    // br.touch() doesn't work...
                    final IErlProject erlProject = ErlangEngine.getInstance()
                            .getModel().getErlangProject(project);
                    compileErl(project, bbr, erlProject.getOutputLocation()
                            .toString(), backend, compilerOptions);
                }
            } catch (final CoreException e) {
                ErlLogger.warn(e);
            }
        }
    }

    public IRpcFuture startCompileErl(final IProject project,
            final BuildResource bres, final String outputDir0,
            final IRpcSite backend, final OtpErlangList compilerOptions,
            final boolean force) {
        final IPath projectPath = project.getLocation();
        final IResource res = bres.getResource();
        final String s = res.getFileExtension();
        if (!ERL.equals(s)) {
            ErlLogger.warn("trying to compile " + res.getName() + "?!?!");
        }

        MarkerUtils.deleteMarkers(res);

        String outputDir;
        outputDir = getRealOutputDir(bres, outputDir0, projectPath);

        final Collection<IPath> includeDirs = getAllIncludeDirs(project);

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

                createTaskMarkers(project, res);
                return BuilderHelper.compileErl(backend, res.getLocation(),
                        outputDir, includeDirs, compilerOptions);

            }
            return null;
        } catch (final Exception e) {
            ErlLogger.warn(e);
            return null;
        }
    }

    private String getRealOutputDir(final BuildResource bres,
            final String outputDir0, final IPath projectPath) {
        String outputDir;
        final String bout = bres.getOutput();
        if (bout == null) {
            outputDir = projectPath.append(outputDir0).toString();
        } else {
            outputDir = bout.startsWith("/") || bout.charAt(1) == ':' ? bout
                    : projectPath.append(bout).toString();
        }
        ensureDirExists(outputDir);
        return outputDir;
    }

    private void createTaskMarkers(final IProject project, final IResource res) {
        BuildQueueProcessor.getInstance().addWork(
                new BuildWorkerInfo(project, res));
    }

    private IPath getBeamForErl(final IResource source) {
        final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                .getErlangProject(source.getProject());
        IPath p = erlProject.getOutputLocation();
        p = p.append(source.getName());
        if (!ERL.equals(p.getFileExtension())) {
            return null;
        }
        final IPath module = p.removeFileExtension();
        final IPath beamPath = module.addFileExtension("beam").setDevice(null);
        return beamPath;
    }

    public IRpcFuture startCompileYrl(final IProject project,
            final IResource resource, final IRpcSite backend,
            final OtpErlangList compilerOptions) {
        // final IPath projectPath = project.getLocation();
        // final OldErlangProjectProperties prefs = new
        // OldErlangProjectProperties(project);

        MarkerUtils.deleteMarkers(resource);
        // try {
        // resource.deleteMarkers(PROBLEM_MARKER, true,
        // IResource.DEPTH_INFINITE);
        // } catch (final CoreException e1) {
        // }

        final IPath erl = getErlForYrl(resource);
        final IResource br = project.findMember(erl);

        // we should check timestamps, but yrl files are rare, so it doesn't
        // matter much

        try {
            if (br != null && br.exists()) {
                try {
                    br.delete(true, null);
                } catch (final Exception e) {
                    ErlLogger.warn(e);
                }
            }

            final String input = resource.getLocation().toString();
            final String output = resource.getLocation().removeFileExtension()
                    .toString();
            return BuilderHelper.compileYrl(backend, input, output);
        } catch (final Exception e) {
            ErlLogger.error(e);
            return null;
        }

    }

    public IPath getErlForYrl(final IResource resource) {
        final IPath path = resource.getProjectRelativePath();
        if (!"yrl".equals(path.getFileExtension())) {
            return null;
        }
        IPath erl = path.removeFileExtension();
        erl = erl.addFileExtension(ERL).setDevice(null);
        return erl;
    }

    public void compileErl(final IProject project,
            final BuildResource resource, final String outputDir,
            final IRpcSite b, final OtpErlangList compilerOptions) {
        final IRpcFuture res = startCompileErl(project, resource, outputDir, b,
                compilerOptions, true);
        if (res == null) {
            ErlLogger.warn("error compiling erl file: "
                    + resource.getResource().getProjectRelativePath());
            return;
        }
        try {
            final OtpErlangObject result = res.checkedGet();
            completeCompile(project, resource.getResource(), result, b,
                    compilerOptions);
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
    }

    public void compileYrl(final IProject project,
            final BuildResource resource, final IRpcSite b,
            final OtpErlangList compilerOptions) {
        final IRpcFuture res = startCompileYrl(project, resource.getResource(),
                b, compilerOptions);
        if (res == null) {
            ErlLogger.warn("error compiling yrl file: "
                    + resource.getResource().getProjectRelativePath());
            return;
        }
        try {
            completeCompile(project, resource.getResource(), res.checkedGet(),
                    b, compilerOptions);
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
    }

    public static IRpcFuture compileErl(final IRpcSite backend, final IPath fn,
            final String outputdir, final Collection<IPath> includedirs,
            final OtpErlangList compilerOptions) {
        final List<String> incs = Lists.newArrayList();
        for (final IPath p : includedirs) {
            incs.add(p.toString());
        }
        try {
            return backend.async_call(ERLIDE_BUILDER, "compile", "sslsx",
                    fn.toString(), outputdir, incs, compilerOptions);
        } catch (final Exception e) {
            ErlLogger.debug(e);
            return null;
        }
    }

    public static IRpcFuture compileYrl(final IRpcSite backend,
            final String fn, final String output) {
        try {
            return backend.async_call(ERLIDE_BUILDER, "compile_yrl", "ss", fn,
                    output);
        } catch (final Exception e) {
            ErlLogger.debug(e);
            return null;
        }
    }

    public static void loadModule(final IProject project, final String module) {
        try {
            final IBackendManager backendManager = BackendCore
                    .getBackendManager();
            for (final IBackend b : backendManager
                    .getExecutionBackends(project)) {
                ErlLogger.debug(":: loading %s in %s", module, b.getName());
                b.getRpcSite().call(ERLIDE_BUILDER, "load", "ao", module,
                        b.getData().shouldLoadOnAllNodes());
                backendManager.moduleLoaded(b, project, module);
            }
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    public static OtpErlangList getSourceClashes(final IRpcSite backend,
            final String[] dirList) throws RpcException {
        final OtpErlangObject res = backend.call(ERLIDE_BUILDER,
                "source_clash", "ls", (Object) dirList);
        if (res instanceof OtpErlangList) {
            return (OtpErlangList) res;
        }
        throw new RpcException("bad result from erlide_builder:source_clash: "
                + res);
    }

    public static OtpErlangList getCodeClashes(final IRpcSite b)
            throws RpcException {
        final OtpErlangList res = (OtpErlangList) b.call(ERLIDE_BUILDER,
                "code_clash", null);
        return res;
    }

    public class SearchVisitor implements IResourceVisitor {

        private IResource fResult;
        String fName;

        public SearchVisitor(final String name, final IProgressMonitor monitor) {
            setResult(null);
            fName = name;
        }

        @Override
        public boolean visit(final IResource resource) throws CoreException {
            if (fName == null) {
                return false;
            }
            if (getResult() != null) {
                return false;
            }
            final IErlProject erlProject = ErlangEngine.getInstance()
                    .getModel().getErlangProject(resource.getProject());
            if (resource.getType() == IResource.FILE
                    && resource.getFileExtension() != null
                    && ERL.equals(resource.getFileExtension())
                    && isInCodePath(resource, erlProject)) {
                final String[] p = resource.getName().split("\\.");
                if (p[0].equals(fName)) {
                    setResult(resource);
                    return false;
                }
            }
            return true;
        }

        public void setResult(final IResource fResult) {
            this.fResult = fResult;
        }

        public IResource getResult() {
            return fResult;
        }
    }

    private static class BuilderVisitor implements IResourceDeltaVisitor,
            IResourceVisitor {

        private final Set<BuildResource> result;
        private final IProgressMonitor monitor;
        private IErlProject erlProject = null;
        private final BuilderHelper helper;

        public BuilderVisitor(final Set<BuildResource> result,
                final IProgressMonitor monitor, final BuilderHelper helper) {
            this.result = result;
            this.monitor = monitor;
            this.helper = helper;
        }

        @Override
        public boolean visit(final IResourceDelta delta) throws CoreException {
            final IResource resource = delta.getResource();
            return visit(resource, delta.getKind(), false);
        }

        @Override
        public boolean visit(final IResource resource) throws CoreException {
            return visit(resource, IResourceDelta.ADDED, true);
        }

        private boolean visit(final IResource resource, final int kind,
                final boolean fullBuild) {
            if (resource.getLocation().toString().contains("lost+found")) {
                return false;
            }
            if (resource.getType() == IResource.PROJECT) {
                erlProject = ErlangEngine.getInstance().getModel()
                        .getErlangProject((IProject) resource);
                return true;
            }
            if (resource.getType() == IResource.FOLDER) {
                return true;
            }

            final IPath path = resource.getParent().getProjectRelativePath();
            final String ext = resource.getFileExtension();
            if (erlProject.getSourceDirs().contains(path)) {
                if (ERL.equals(ext)) {
                    handleErlFile(kind, resource);
                    return false;
                }
                if ("yrl".equals(ext)) {
                    handleYrlFile(kind, resource);
                    return false;
                }
            }
            if (erlProject.getIncludeDirs().contains(path) && "hrl".equals(ext)) {
                try {
                    handleHrlFile(kind, resource, fullBuild);
                } catch (final ErlModelException e) {
                    ErlLogger.warn(e);
                }
                return false;
            }
            if (erlProject.getOutputLocation().equals(path)
                    && "beam".equals(ext)) {
                try {
                    handleBeamFile(kind, resource);
                } catch (final CoreException e) {
                    ErlLogger.warn(e);
                }
                return false;
            }
            return true;
        }

        private void handleBeamFile(final int kind, final IResource resource)
                throws CoreException {
            switch (kind) {
            case IResourceDelta.ADDED:
            case IResourceDelta.CHANGED:
                break;
            case IResourceDelta.REMOVED:
                final IResource source = findCorrespondingSource(resource);
                if (source != null) {
                    final BuildResource bres = new BuildResource(source);
                    result.add(bres);
                    monitor.worked(1);
                }
                break;
            }
        }

        public IResource findCorrespondingSource(final IResource beam)
                throws CoreException {
            final String[] p = beam.getName().split("\\.");
            final SearchVisitor searcher = helper.new SearchVisitor(p[0], null);
            beam.getProject().accept(searcher);
            final IResource source = searcher.getResult();
            return source;
        }

        private void handleYrlFile(final int kind, final IResource resource) {
            switch (kind) {
            case IResourceDelta.ADDED:
            case IResourceDelta.CHANGED:
                final BuildResource bres = new BuildResource(resource);
                result.add(bres);
                monitor.worked(1);
                break;

            case IResourceDelta.REMOVED:
                MarkerUtils.deleteMarkers(resource);

                final IPath erl = helper.getErlForYrl(resource);
                final IResource br = resource.getProject().findMember(erl);
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

        private void handleHrlFile(final int kind, final IResource resource,
                final boolean fullBuild) throws ErlModelException {
            switch (kind) {
            case IResourceDelta.ADDED:
            case IResourceDelta.REMOVED:
            case IResourceDelta.CHANGED:
                final int n = result.size();
                if (!fullBuild) {
                    helper.addDependents(resource, resource.getProject(),
                            result);
                }
                monitor.worked(result.size() - n);
                break;
            }
        }

        private void handleErlFile(final int kind, final IResource resource) {
            switch (kind) {
            case IResourceDelta.ADDED:
            case IResourceDelta.CHANGED:
                final BuildResource bres = new BuildResource(resource);
                result.add(bres);
                monitor.worked(1);
                break;
            case IResourceDelta.REMOVED:
                MarkerUtils.deleteMarkers(resource);
                IPath beam = erlProject.getOutputLocation();
                final IPath module = beam.append(resource.getName())
                        .removeFileExtension();
                beam = module.addFileExtension("beam").setDevice(null);
                final IResource br = resource.getProject().findMember(beam);
                if (br != null) {
                    try {
                        br.delete(true, null);
                    } catch (final Exception e) {
                        ErlLogger.warn(e);
                    }
                }

                // was it derived from a yrl?
                final IPath yrlpath = resource.getProjectRelativePath()
                        .removeFileExtension().addFileExtension("yrl");
                final IResource yrl = resource.getProject().findMember(yrlpath);
                if (yrl != null) {
                    final BuildResource bres2 = new BuildResource(yrl);
                    result.add(bres2);
                    monitor.worked(1);
                }

                break;
            }
        }

    }
}
