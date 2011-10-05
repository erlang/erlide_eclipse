/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.internal.model.erlang;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.erlide.core.common.CommonUtils;
import org.erlide.core.internal.model.root.ErlModel;
import org.erlide.core.internal.model.root.ErlScanner;
import org.erlide.core.internal.model.root.Openable;
import org.erlide.core.internal.model.root.SourceRange;
import org.erlide.core.model.erlang.ErlangToolkit;
import org.erlide.core.model.erlang.IErlAttribute;
import org.erlide.core.model.erlang.IErlComment;
import org.erlide.core.model.erlang.IErlExport;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlImport;
import org.erlide.core.model.erlang.IErlMember;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.IErlPreprocessorDef;
import org.erlide.core.model.erlang.IErlTypespec;
import org.erlide.core.model.erlang.ModuleKind;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlToken;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlExternal;
import org.erlide.core.model.root.IErlFolder;
import org.erlide.core.model.root.IErlModel;
import org.erlide.core.model.root.IErlParser;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.model.root.IErlScanner;
import org.erlide.core.model.root.IParent;
import org.erlide.core.model.root.ISourceRange;
import org.erlide.core.model.root.ISourceReference;
import org.erlide.core.model.util.ErlangFunction;
import org.erlide.core.model.util.ErlangIncludeFile;
import org.erlide.jinterface.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.google.common.base.Predicate;
import com.google.common.collect.Lists;

public class ErlModule extends Openable implements IErlModule {

    private static final OtpErlangAtom EXPORT_ALL = new OtpErlangAtom(
            "export_all");
    private long timestamp = IResource.NULL_STAMP;
    private IFile fFile;
    private final ModuleKind moduleKind;
    protected String path;
    private String initialText;
    private boolean parsed;
    private final String scannerName;
    private IErlScanner scanner;
    private final boolean useCaches;
    private final Collection<IErlComment> comments;

    public ErlModule(final IParent parent, final String name,
            final String initialText, final IFile file, final String path,
            final boolean useCaches) {
        super(parent, name);
        fFile = file;
        moduleKind = ModuleKind.nameToModuleKind(name);
        this.path = path;
        this.initialText = initialText;
        parsed = false;
        scannerName = ErlangToolkit.createScannerModuleName(this);
        scanner = null;
        this.useCaches = useCaches;
        comments = Lists.newArrayList();
        if (ErlModel.verbose) {
            final IErlElement element = (IErlElement) parent;
            final String parentName = element.getName();
            ErlLogger.debug("...creating " + parentName + "/" + getName() + " "
                    + moduleKind);
        }
        getModelCache().putModule(this);
    }

    public boolean internalBuildStructure(final IProgressMonitor pm) {
        if (scanner == null) {
            parsed = false;
        }
        if (scanner == null) {
            // There are two places that we make the initial scanner... this
            // is one
            getScanner();
        }
        getScanner();
        try {
            final IErlParser parser = getModel().getParser();
            parsed = parser.parse(this, scannerName, !parsed, getFilePath(),
                    useCaches);
        } finally {
            disposeScanner();
        }
        return parsed;
    }

    @Override
    protected synchronized boolean buildStructure(final IProgressMonitor pm)
            throws ErlModelException {
        if (internalBuildStructure(pm)) {
            final IErlModel model = getModel();
            if (model != null) {
                model.notifyChange(this);
            }
            final IResource r = getResource();
            if (r instanceof IFile) {
                timestamp = ((IFile) r).getLocalTimeStamp();
            } else {
                timestamp = IResource.NULL_STAMP;
            }
            return true;
        }
        return false;
    }

    @Override
    public String getFilePath() {
        if (fFile != null) {
            final IPath location = fFile.getLocation();
            if (location != null) {
                return location.toString();
            }
        }
        return path;
    }

    public IErlElement getElementAt(final int position)
            throws ErlModelException {
        return getModel().innermostThat(this, new Predicate<IErlElement>() {
            public boolean apply(final IErlElement e) {
                if (e instanceof ISourceReference) {
                    final ISourceReference ch = (ISourceReference) e;
                    ISourceRange r;
                    r = ch.getSourceRange();
                    if (r != null && r.hasPosition(position)) {
                        return true;
                    }
                }
                return false;
            }
        });
    }

    public IErlMember getElementAtLine(final int lineNumber) {
        return (IErlMember) getModel().innermostThat(this,
                new Predicate<IErlElement>() {
                    public boolean apply(final IErlElement e) {
                        if (e instanceof ISourceReference) {
                            final ISourceReference sr = (ISourceReference) e;
                            if (sr.getLineStart() <= lineNumber
                                    && sr.getLineEnd() >= lineNumber) {
                                return true;
                            }
                        }
                        return false;
                    }
                });
    }

    public ModuleKind getModuleKind() {
        return moduleKind;
    }

    @Override
    public IResource getResource() {
        return getCorrespondingResource();
    }

    @Override
    public IResource getCorrespondingResource() {
        return fFile;
    }

    public ISourceRange getSourceRange() throws ErlModelException {
        return new SourceRange(0, 0);
    }

    @Override
    protected boolean hasBuffer() {
        return true;
    }

    public void addComment(final IErlComment c) {
        comments.add(c);
    }

    public Collection<IErlComment> getComments() {
        return Collections.unmodifiableCollection(comments);
    }

    @Override
    public void setChildren(final Collection<? extends IErlElement> children) {
        comments.clear();
        super.setChildren(children);
    }

    public synchronized long getTimestamp() {
        return timestamp;
    }

    public IErlImport findImport(final ErlangFunction function) {
        try {
            for (final IErlElement e : getChildrenOfKind(Kind.IMPORT)) {
                if (e instanceof IErlImport) {
                    final IErlImport ei = (IErlImport) e;
                    if (ei.hasFunction(function)) {
                        return ei;
                    }
                }
            }
        } catch (final ErlModelException e) {
        }
        return null;
    }

    public IErlExport findExport(final ErlangFunction function) {
        try {
            for (final IErlElement e : getChildrenOfKind(Kind.EXPORT)) {
                if (e instanceof IErlExport) {
                    final IErlExport ee = (IErlExport) e;
                    if (ee.hasFunction(function)) {
                        return ee;
                    }
                }
            }
        } catch (final ErlModelException e) {
        }
        return null;
    }

    public IErlFunction findFunction(final ErlangFunction function) {
        try {
            for (final IErlElement fun : getChildren()) {
                if (fun instanceof IErlFunction) {
                    final IErlFunction f = (IErlFunction) fun;
                    if (f.getName().equals(function.name)
                            && (function.arity < 0 || f.getArity() == function.arity)) {
                        return f;
                    }
                }
            }
        } catch (final ErlModelException e) {
            // ignore
        }
        return null;
    }

    public IErlTypespec findTypespec(final String typeName) {
        try {
            for (final IErlElement child : getChildren()) {
                if (child instanceof IErlTypespec) {
                    final IErlTypespec typespec = (IErlTypespec) child;
                    if (typespec.getName().equals(typeName)) {
                        return typespec;
                    }
                }
            }
        } catch (final ErlModelException e) {
            // ignore
        }
        return null;
    }

    public IErlPreprocessorDef findPreprocessorDef(final String definedName,
            final Kind kind) {
        try {
            for (final IErlElement m : getChildren()) {
                if (m instanceof IErlPreprocessorDef) {
                    final IErlPreprocessorDef pd = (IErlPreprocessorDef) m;
                    if (pd.getKind() == kind
                            && pd.getDefinedName().equals(definedName)) {
                        return pd;
                    }
                }
            }
        } catch (final ErlModelException e) {
        }
        return null;
    }

    public Collection<ErlangIncludeFile> getIncludeFiles()
            throws ErlModelException {
        if (!isStructureKnown()) {
            open(null);
        }
        final List<ErlangIncludeFile> r = new ArrayList<ErlangIncludeFile>(0);
        for (final IErlElement m : getChildren()) {
            if (m instanceof IErlAttribute) {
                final IErlAttribute a = (IErlAttribute) m;
                final OtpErlangObject v = a.getValue();
                if (v instanceof OtpErlangString) {
                    final String s = ((OtpErlangString) v).stringValue();
                    if ("include".equals(a.getName())) {
                        r.add(new ErlangIncludeFile(false, s));
                    } else if ("include_lib".equals(a.getName())) {
                        r.add(new ErlangIncludeFile(true, s));
                    }
                }
            }
        }
        return r;
    }

    public Collection<IErlImport> getImports() {
        final List<IErlImport> result = new ArrayList<IErlImport>();
        try {
            for (final IErlElement e : getChildren()) {
                if (e instanceof IErlImport) {
                    final IErlImport ei = (IErlImport) e;
                    result.add(ei);
                }
            }
        } catch (final ErlModelException e) {
        }
        return result;
    }

    public synchronized void reconcileText(final int offset,
            final int removeLength, final String newText,
            final IProgressMonitor mon) {
        if (scanner == null) {
            // There are two places that we make the initial scanner... this
            // is one too
            getScanner();
        }
        getScanner();
        if (scanner != null) {
            scanner.replaceText(offset, removeLength, newText);
        }
        if (mon != null) {
            mon.worked(1);
        }
        setStructureKnown(false);
        disposeScanner();
    }

    public synchronized void postReconcile(final IProgressMonitor mon) {
        try {
            open(mon);
        } catch (final ErlModelException e) {
            ErlLogger.warn(e);
        }
        if (mon != null) {
            mon.worked(1);
        }
    }

    public synchronized void initialReconcile() {
        // currently unused
        // Note that the ErlReconciler doesn't send the first full-text
        // reconcile that the built-in reconciler does
    }

    public synchronized void finalReconcile() {
        // currently unused
    }

    public String getModuleName() {
        return CommonUtils.withoutExtension(getName());
    }

    public void disposeScanner() {
        if (scanner == null) {
            return;
        }
        final IErlScanner s = scanner;
        if (s.willDispose()) {
            scanner = null;
        }
        s.dispose();
        setStructureKnown(false);
    }

    // public synchronized void disposeParser() {
    // final Backend b = ErlangCore.getBackendManager().getIdeBackend();
    // ErlideNoparse.destroy(b, getModuleName());
    // disposeScanner();
    // setStructureKnown(false);
    // parsed = false;
    // }

    public Kind getKind() {
        return Kind.MODULE;
    }

    @Override
    public void dispose() {
        disposeScanner();
        getModel().removeModule(this);
    }

    public Set<IErlModule> getDirectDependentModules() throws ErlModelException {
        final Set<IErlModule> result = new HashSet<IErlModule>();
        final IErlProject project = getProject();
        for (final IErlModule module : project.getModules()) {
            final boolean wasOpen = module.isOpen();
            if (!wasOpen) {
                module.open(null);
            }
            final Collection<ErlangIncludeFile> incs = module.getIncludeFiles();
            for (final ErlangIncludeFile inc : incs) {
                if (inc.getFilename().equals(getName())) {
                    result.add(module);
                    break;
                }
            }
            if (!wasOpen) {
                module.close();
            }
        }
        return result;
    }

    public Set<IErlModule> getAllDependentModules() throws CoreException {
        final Set<IErlModule> result = new HashSet<IErlModule>();
        final IErlProject project = getProject();
        for (final IErlModule module : project.getModules()) {
            final List<IErlModule> allIncludedFiles = module
                    .findAllIncludedFiles();
            if (allIncludedFiles.contains(this)) {
                result.add(module);
            }
        }
        return result;
    }

    public synchronized void resetAndCacheScannerAndParser(final String newText)
            throws ErlModelException {
        while (scanner != null) {
            disposeScanner();
        }
        initialText = newText;
        parsed = false;
        setStructureKnown(false);
        final boolean built = buildStructure(null);
        setStructureKnown(built);
    }

    public ErlToken getScannerTokenAt(final int offset) {
        if (scanner != null) {
            return scanner.getTokenAt(offset);
        }
        return null;
    }

    public void setResource(final IFile file) {
        fFile = file;
    }

    @Override
    public String getLabelString() {
        return getName();
    }

    public void getScanner() {
        if (scanner == null) {
            scanner = getNewScanner();
        }
        scanner.addRef();
    }

    private IErlScanner getNewScanner() {
        final String filePath = getFilePath();
        if (filePath == null) {
            return null;
        }
        if (initialText == null) {
            initialText = "";
        }
        return new ErlScanner(scannerName, initialText, filePath, useCaches);
    }

    public Collection<IErlPreprocessorDef> getPreprocessorDefs(final Kind kind) {
        final List<IErlPreprocessorDef> result = Lists.newArrayList();
        try {
            for (final IErlElement e : getChildren()) {
                if (e instanceof IErlPreprocessorDef) {
                    final IErlPreprocessorDef pd = (IErlPreprocessorDef) e;
                    if (pd.getKind() == kind || kind == Kind.ERROR) {
                        result.add(pd);
                    }
                }
            }
        } catch (final ErlModelException e) {
        }
        return result;
    }

    public List<IErlModule> findAllIncludedFiles() throws CoreException {
        final List<IErlModule> checked = Lists.newArrayList();
        checked.add(this);
        return findAllIncludedFiles(checked);
    }

    public List<IErlModule> findAllIncludedFiles(final List<IErlModule> checked)
            throws CoreException {
        final List<IErlModule> includedFilesForModule = ErlModel
                .getErlModelCache().getIncludedFilesForModule(this);
        if (includedFilesForModule != null && !includedFilesForModule.isEmpty()) {
            return includedFilesForModule;
        }
        final List<IErlModule> result = Lists.newArrayList();
        final Collection<ErlangIncludeFile> includedFiles = getIncludeFiles();
        final IErlProject project = getProject();
        if (project == null) {
            return result;
        }
        final Collection<IErlModule> includes = project.getIncludes();
        includes.addAll(getLocalIncludes());
        Collection<IErlModule> externalIncludes = null;
        Collection<IErlModule> referencedIncludes = null;
        Collection<IErlModule> modules = null;
        for (final ErlangIncludeFile includeFile : includedFiles) {
            final String includeFileName = includeFile.getFilenameLastPart();
            if (findAllIncludedFilesAux(checked, result, includes,
                    includeFileName)) {
                continue;
            }
            if (referencedIncludes == null) {
                referencedIncludes = Lists.newArrayList();
                final Collection<IErlProject> referencedProjects = project
                        .getReferencedProjects();
                for (final IErlProject referencedProject : referencedProjects) {
                    referencedIncludes.addAll(referencedProject.getIncludes());
                }
            }
            if (findAllIncludedFilesAux(checked, result, referencedIncludes,
                    includeFileName)) {
                continue;
            }
            if (externalIncludes == null) {
                externalIncludes = project.getExternalIncludes();
            }
            if (findAllIncludedFilesAux(checked, result, externalIncludes,
                    includeFileName)) {
                continue;
            }
            if (modules == null) {
                modules = project.getModules();
            }
            findAllIncludedFilesAux(checked, result, modules, includeFileName);
        }
        ErlModel.getErlModelCache().putIncludedFilesForModule(this, result);
        return result;
    }

    private Collection<IErlModule> getLocalIncludes() throws ErlModelException {
        final List<IErlModule> result = Lists.newArrayList();
        final IParent parent = getParent();
        for (final IErlElement child : parent.getChildrenOfKind(Kind.MODULE)) {
            if (child instanceof IErlModule
                    && ModuleKind.nameToModuleKind(child.getName()) == ModuleKind.HRL) {
                result.add((IErlModule) child);
            }
        }
        return result;
    }

    private boolean findAllIncludedFilesAux(final List<IErlModule> checked,
            final List<IErlModule> result,
            final Collection<IErlModule> includes, final String includeFileName)
            throws CoreException {
        for (final IErlModule include : includes) {
            if (include.getName().equals(includeFileName)) {
                if (include.getParent() instanceof IErlExternal) {
                    result.add(findExternalIncludeInOpenProjects(include));
                } else {
                    result.add(include);
                }
                final ErlModule m = (ErlModule) include;
                result.addAll(m.findAllIncludedFiles(checked));
                return true;
            }
        }
        return false;
    }

    public static IErlModule findExternalIncludeInOpenProjects(
            final IErlModule externalInclude) throws CoreException {
        final String filePath = externalInclude.getFilePath();
        final Collection<IErlProject> projects = externalInclude.getModel()
                .getErlangProjects();
        for (final IErlProject project : projects) {
            final Collection<IErlModule> includes = project.getIncludes();
            for (final IErlModule include : includes) {
                if (include.getFilePath().equals(filePath)) {
                    return include;
                }
            }
        }
        return externalInclude;
    }

    public boolean isOnSourcePath() {
        final IParent parent = getParent();
        if (parent instanceof IErlFolder) {
            final IErlFolder folder = (IErlFolder) parent;
            return folder.isOnSourcePath();
        }
        if (checkPath(getProject().getSourceDirs())) {
            return true;
        }
        return false;
    }

    public boolean isOnIncludePath() {
        final IParent parent = getParent();
        if (parent instanceof IErlFolder) {
            final IErlFolder folder = (IErlFolder) parent;
            return folder.isOnIncludePath();
        }
        if (checkPath(getProject().getIncludeDirs())) {
            return true;
        }
        return false;
    }

    private boolean checkPath(final Collection<IPath> dirs) {
        final String thePath = getFilePath();
        if (thePath != null) {
            final IPath p = new Path(thePath).removeLastSegments(1);
            for (final IPath dir : dirs) {
                if (dir.equals(p)) {
                    return true;
                }
            }
        }
        return false;
    }

    public String getText() {
        getScanner();
        final String s = scanner.getText();
        disposeScanner();
        return s;
    }

    public boolean exportsAllFunctions() {
        try {
            for (final IErlElement e : getChildren()) {
                if (e instanceof IErlAttribute) {
                    final IErlAttribute attr = (IErlAttribute) e;
                    if (attr.getName().equals("compile")) {
                        final OtpErlangObject value = attr.getValue();
                        if (value != null && value.equals(EXPORT_ALL)) {
                            return true;
                        }
                    }
                }
            }
        } catch (final ErlModelException e) {
            // ignore
        }
        return false;
    }

}
