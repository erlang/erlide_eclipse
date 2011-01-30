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
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.builder.internal.BuilderVisitor;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlModule.ModuleKind;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IOldErlangProjectProperties;
import org.erlide.core.erlang.util.ErlangIncludeFile;
import org.erlide.core.erlang.util.PluginUtils;
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

public final class BuilderHelper {

    public BuilderHelper() {
    }

    public static boolean isDebugging() {
        if (ErlangPlugin.getDefault() == null) {
            return false;
        }
        return ErlangPlugin.getDefault().isDebugging()
                && Platform.getDebugOption("org.erlide.core/debug/builder")
                        .equals("true");
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
        final IOldErlangProjectProperties prefs = ErlangCore
                .getProjectProperties(project);
        final Collection<IPath> incs = prefs.getIncludeDirs();
        final IPathVariableManager pvm = ResourcesPlugin.getWorkspace()
                .getPathVariableManager();
        for (IPath inc : incs) {
            inc = PluginUtils.resolvePVMPath(pvm, inc);
            if (inc.isAbsolute()) {
                includeDirs.add(inc);
            } else {
                final IFolder folder = project.getFolder(inc);
                if (folder != null) {
                    final IPath location = folder.getLocation();
                    includeDirs.add(location);
                }
            }
        }
        return includeDirs;
    }

    boolean isInCodePath(final IResource resource,
            final IOldErlangProjectProperties prefs) {
        final IPath projectPath = resource.getProject().getFullPath();
        final Collection<IPath> srcs = prefs.getSourceDirs();
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
        final IErlProject eprj = ErlangCore.getModel().findProject(my_project);
        if (eprj != null) {
            final Collection<IErlModule> ms = eprj.getModules();
            for (final IErlModule m : ms) {
                m.getScanner();
                final Collection<ErlangIncludeFile> incs = m.getIncludedFiles();
                for (final ErlangIncludeFile ifile : incs) {
                    if (samePath(ifile.getFilename(), resource.getName())) {
                        if (m.getModuleKind() == ModuleKind.ERL) {
                            final BuildResource bres = new BuildResource(
                                    m.getResource());
                            result.add(bres);
                        }
                        break;
                    }
                }
                m.disposeScanner();
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

    public void checkForClashes(final Backend backend, final IProject project) {
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
                if (r1 != null || r2 != null) {
                    MarkerUtils.addMarker(project, null, project,
                            "Code clash between " + f1 + " and " + f2, 0,
                            IMarker.SEVERITY_WARNING, "");
                }
            }

        } catch (final Exception e) {
        }
        try {
            final IOldErlangProjectProperties pp = ErlangCore
                    .getProjectProperties(project);
            final Collection<IPath> sd = pp.getSourceDirs();
            final String[] dirList = new String[sd.size()];
            int j = 0;
            for (final IPath sp : sd) {
                dirList[j++] = project.getLocation().toPortableString() + "/"
                        + sp;
            }
            final OtpErlangList res = ErlideBuilder.getSourceClashes(backend,
                    dirList);
            for (int i = 0; i < res.arity(); i++) {
                final OtpErlangTuple t = (OtpErlangTuple) res.elementAt(i);
                final String f1 = ((OtpErlangString) t.elementAt(0))
                        .stringValue();
                final String f2 = ((OtpErlangString) t.elementAt(1))
                        .stringValue();
                MarkerUtils.addMarker(project, null, project,
                        "Duplicated module name in " + f1 + " and " + f2, 0,
                        IMarker.SEVERITY_ERROR, "");
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
            final IErlProject eprj = ErlangCore.getModel().findProject(project);
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
            boolean shouldCompile, final IErlProject eprj)
            throws ErlModelException {
        final IErlModule m = eprj.getModule(source.getName());
        if (m != null) {
            m.getScanner();
            final Collection<ErlangIncludeFile> incs = m.getIncludedFiles();
            for (final ErlangIncludeFile ifile : incs) {
                final IResource rifile = findResourceByName(project,
                        ifile.getFilename());
                if (rifile != null
                        && rifile.getLocalTimeStamp() > beam
                                .getLocalTimeStamp()) {
                    shouldCompile = true;
                    break;
                }
            }
            m.disposeScanner();
        }
        return shouldCompile;
    }

    public static boolean samePath(final String p1, final String p2) {
        final boolean WINDOWS = java.io.File.separatorChar == '\\';
        if (WINDOWS) {
            return p1.equalsIgnoreCase(p2);
        } else {
            return p1.equals(p2);
        }
    }

    private final static class FindResourceVisitor implements IResourceVisitor {
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
                final int theHow) {
            if (theHow == FIND_BY_NAME) {
                return samePath(resource.getName(), s);
            } else if (theHow == FIND_BY_LOCATION) {
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

    public IResource findResourceByName(final IContainer container,
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

    public void refreshOutputDir(final IProject project) throws CoreException {
        final IOldErlangProjectProperties prefs = ErlangCore
                .getProjectProperties(project);
        final IPath outputDir = prefs.getOutputDir();
        final IResource ebinDir = project.findMember(outputDir);
        if (ebinDir != null) {
            ebinDir.refreshLocal(IResource.DEPTH_ONE, null);
        }
    }

    public void completeCompile(final IProject project, final IResource source,
            final OtpErlangObject r, final Backend backend,
            final OtpErlangList compilerOptions) {
        if (r == null) {
            MarkerUtils.addProblemMarker(source, null, null,
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
            MarkerUtils.addErrorMarkers(source, l);
        } else {
            ErlLogger.warn("bad result from builder: %s", t);
        }

        completeCompileForYrl(project, source, backend, compilerOptions);
    }

    private void completeCompileForYrl(final IProject project,
            final IResource source, final Backend backend,
            final OtpErlangList compilerOptions) {
        final IPath erl = getErlForYrl(source);
        if (erl != null) {
            try {
                source.getParent().refreshLocal(IResource.DEPTH_ONE, null);
                final IResource br = project.findMember(erl);
                if (br != null) {
                    br.setDerived(true);
                    final BuildResource bbr = new BuildResource(br);
                    // br.touch() doesn't work...
                    final IOldErlangProjectProperties prefs = ErlangCore
                            .getProjectProperties(project);
                    compileErl(project, bbr, prefs.getOutputDir().toString(),
                            backend, compilerOptions);
                }
            } catch (final CoreException e) {
                ErlLogger.warn(e);
            }
        }
    }

    public RpcFuture startCompileErl(final IProject project,
            final BuildResource bres, final String outputDir0,
            final Backend backend, final OtpErlangList compilerOptions,
            final boolean force) {
        final IPath projectPath = project.getLocation();
        final IResource res = bres.getResource();
        final String s = res.getFileExtension();
        if (!"erl".equals(s)) {
            ErlLogger.warn("trying to compile " + res.getName() + "?!?!");
        }

        MarkerUtils.deleteMarkers(res);

        final String outputDir = bres.getOutput() == null ? projectPath.append(
                outputDir0).toString()
                : bres.getOutput().startsWith("/") ? bres.getOutput()
                        : projectPath.append(bres.getOutput()).toString();
        ensureDirExists(outputDir);

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
                return ErlideBuilder.compileErl(backend, res.getLocation(),
                        outputDir, includeDirs, compilerOptions);

            } else {
                return null;
            }
        } catch (final Exception e) {
            ErlLogger.warn(e);
            return null;
        }
    }

    private void createTaskMarkers(final IProject project, final IResource res) {
        final Job job = new Job("tasks") {
            @Override
            protected IStatus run(final IProgressMonitor monitor) {
                MarkerUtils.createTaskMarkers(project, res);
                return Status.OK_STATUS;
            }
        };
        job.setSystem(true);
        job.setPriority(Job.BUILD);
        job.setRule(res);
        job.schedule();
    }

    private IPath getBeamForErl(final IResource source) {
        final IOldErlangProjectProperties prefs = ErlangCore
                .getProjectProperties(source.getProject());
        IPath p = prefs.getOutputDir();
        p = p.append(source.getName());
        if (!"erl".equals(p.getFileExtension())) {
            return null;
        }
        final IPath module = p.removeFileExtension();
        final IPath beamPath = module.addFileExtension("beam").setDevice(null);
        return beamPath;
    }

    public RpcFuture startCompileYrl(final IProject project,
            final IResource resource, final Backend backend,
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

    public IPath getErlForYrl(final IResource resource) {
        final IPath path = resource.getProjectRelativePath();
        if (!"yrl".equals(path.getFileExtension())) {
            return null;
        }
        IPath erl = path.removeFileExtension();
        erl = erl.addFileExtension("erl").setDevice(null);
        return erl;
    }

    public void compileErl(final IProject project,
            final BuildResource resource, final String outputDir,
            final Backend b, final OtpErlangList compilerOptions) {
        final RpcFuture res = startCompileErl(project, resource, outputDir, b,
                compilerOptions, true);
        if (res == null) {
            ErlLogger.warn("error compiling erl file: "
                    + resource.getResource().getProjectRelativePath());
            return;
        }
        try {
            completeCompile(project, resource.getResource(), res.get(), b,
                    compilerOptions);
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
    }

    public void compileYrl(final IProject project,
            final BuildResource resource, final Backend b,
            final OtpErlangList compilerOptions) {
        final RpcFuture res = startCompileYrl(project, resource.getResource(),
                b, compilerOptions);
        if (res == null) {
            ErlLogger.warn("error compiling yrl file: "
                    + resource.getResource().getProjectRelativePath());
            return;
        }
        try {
            completeCompile(project, resource.getResource(), res.get(), b,
                    compilerOptions);
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
    }

    public class SearchVisitor implements IResourceVisitor {

        private IResource fResult;
        String fName;

        public SearchVisitor(final String name, final IProgressMonitor monitor) {
            setResult(null);
            fName = name;
        }

        public boolean visit(final IResource resource) throws CoreException {
            if (fName == null) {
                return false;
            }
            if (getResult() != null) {
                return false;
            }
            final IOldErlangProjectProperties prefs = ErlangCore
                    .getProjectProperties(resource.getProject());
            if (resource.getType() == IResource.FILE
                    && resource.getFileExtension() != null
                    && "erl".equals(resource.getFileExtension())
                    && isInCodePath(resource, prefs)) {
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

}
