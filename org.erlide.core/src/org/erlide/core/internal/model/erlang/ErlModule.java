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
import org.erlide.core.internal.model.root.ErlModel;
import org.erlide.core.internal.model.root.ModelConfig;
import org.erlide.core.internal.model.root.Openable;
import org.erlide.core.model.erlang.ErlToken;
import org.erlide.core.model.erlang.IErlAttribute;
import org.erlide.core.model.erlang.IErlComment;
import org.erlide.core.model.erlang.IErlExport;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlImport;
import org.erlide.core.model.erlang.IErlMember;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.IErlParser;
import org.erlide.core.model.erlang.IErlPreprocessorDef;
import org.erlide.core.model.erlang.IErlScanner;
import org.erlide.core.model.erlang.IErlTypespec;
import org.erlide.core.model.erlang.ISourceRange;
import org.erlide.core.model.erlang.ISourceReference;
import org.erlide.core.model.erlang.ModuleKind;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlExternal;
import org.erlide.core.model.root.IErlFolder;
import org.erlide.core.model.root.IErlModel;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.model.root.IParent;
import org.erlide.core.model.util.ErlangFunction;
import org.erlide.core.model.util.ErlangIncludeFile;
import org.erlide.jinterface.ErlLogger;
import org.erlide.utils.SystemUtils;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.google.common.base.Predicate;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

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
        scannerName = createScannerName();
        scanner = null;
        this.useCaches = useCaches;
        comments = Lists.newArrayList();
        if (ModelConfig.verbose) {
            final IErlElement element = (IErlElement) parent;
            final String parentName = element.getName();
            ErlLogger.debug("...creating " + parentName + "/" + getName() + " "
                    + moduleKind);
        }
        if (useCaches) {
            getModelCache().putModule(this);
        }
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

    @Override
    public IErlElement getElementAt(final int position)
            throws ErlModelException {
        return getModel().innermostThat(this, new Predicate<IErlElement>() {
            @Override
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

    @Override
    public IErlMember getElementAtLine(final int lineNumber) {
        return (IErlMember) getModel().innermostThat(this,
                new Predicate<IErlElement>() {
                    @Override
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

    @Override
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

    // public void addComment(final IErlComment c) {
    // comments.add(c);
    // }

    @Override
    public void setComments(final Collection<? extends IErlComment> comments) {
        synchronized (getModelLock()) {
            this.comments.clear();
            if (comments != null) {
                this.comments.addAll(comments);
            }
        }
    }

    @Override
    public Collection<IErlComment> getComments() {
        synchronized (getModelLock()) {
            return Collections.unmodifiableCollection(comments);
        }
    }

    @Override
    public synchronized long getTimestamp() {
        return timestamp;
    }

    @Override
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

    @Override
    public IErlFunction findFunction(final ErlangFunction function) {
        try {
            for (final IErlElement fun : getChildrenOfKind(Kind.FUNCTION)) {
                if (fun instanceof IErlFunction) {
                    final IErlFunction f = (IErlFunction) fun;
                    if (f.getName().equals(function.name)
                            && (function.arity < 0 || f.getArity() == function.arity)) {
                        return f;
                    }
                }
            }
        } catch (final ErlModelException e) { // ignore
        }
        return null;
    }

    @Override
    public IErlTypespec findTypespec(final String typeName) {
        try {
            for (final IErlElement child : getChildrenOfKind(Kind.TYPESPEC)) {
                if (child instanceof IErlTypespec) {
                    final IErlTypespec typespec = (IErlTypespec) child;
                    if (typespec.getName().equals(typeName)) {
                        return typespec;
                    }
                }
            }
        } catch (final ErlModelException e) { // ignore
        }
        return null;
    }

    @Override
    public IErlPreprocessorDef findPreprocessorDef(final String definedName,
            final Kind kind) {
        synchronized (getModelLock()) {
            for (final IErlElement m : internalGetChildren()) {
                if (m instanceof IErlPreprocessorDef) {
                    final IErlPreprocessorDef pd = (IErlPreprocessorDef) m;
                    if (pd.getKind() == kind
                            && pd.getDefinedName().equals(definedName)) {
                        return pd;
                    }
                }
            }
        }
        return null;
    }

    @Override
    public Collection<ErlangIncludeFile> getIncludeFiles()
            throws ErlModelException {
        if (!isStructureKnown()) {
            open(null);
        }
        final List<ErlangIncludeFile> r = Lists.newArrayList();
        synchronized (getModelLock()) {
            for (final IErlElement m : internalGetChildren()) {
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
        }
        return r;
    }

    @Override
    public Collection<IErlImport> getImports() {
        final List<IErlImport> result = new ArrayList<IErlImport>();
        synchronized (getModelLock()) {
            for (final IErlElement e : internalGetChildren()) {
                if (e instanceof IErlImport) {
                    final IErlImport ei = (IErlImport) e;
                    result.add(ei);
                }
            }
        }
        return result;
    }

    @Override
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

    @Override
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

    @Override
    public synchronized void initialReconcile() {
        // currently unused
        // Note that the ErlReconciler doesn't send the first full-text
        // reconcile that the built-in reconciler does
    }

    @Override
    public synchronized void finalReconcile() {
        // currently unused
    }

    @Override
    public String getModuleName() {
        return SystemUtils.withoutExtension(getName());
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

    @Override
    public Kind getKind() {
        return Kind.MODULE;
    }

    @Override
    public void dispose() {
        disposeScanner();
        getModel().removeModule(this);
    }

    @Override
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

    @Override
    public Set<IErlModule> getAllDependentModules() throws CoreException {
        final Set<IErlModule> result = new HashSet<IErlModule>();
        final IErlProject project = getProject();
        for (final IErlModule module : project.getModules()) {
            final Collection<IErlModule> allIncludedFiles = module
                    .findAllIncludedFiles();
            if (allIncludedFiles.contains(this)) {
                result.add(module);
            }
        }
        return result;
    }

    @Override
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

    @Override
    public ErlToken getScannerTokenAt(final int offset) {
        if (scanner != null) {
            return scanner.getTokenAt(offset);
        }
        return null;
    }

    @Override
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
        if (scanner != null) {
            scanner.addRef();
        }
    }

    private IErlScanner getNewScanner() {
        final String filePath = getFilePath();
        if (filePath == null) {
            return null;
        }
        if (initialText == null) {
            initialText = "";
        }
        return getModel().getToolkit().createScanner(scannerName, initialText,
                filePath, useCaches);
    }

    @Override
    public Collection<IErlPreprocessorDef> getPreprocessorDefs(final Kind kind) {
        final List<IErlPreprocessorDef> result = Lists.newArrayList();
        synchronized (getModelLock()) {
            for (final IErlElement e : internalGetChildren()) {
                if (e instanceof IErlPreprocessorDef) {
                    final IErlPreprocessorDef pd = (IErlPreprocessorDef) e;
                    if (pd.getKind() == kind || kind == Kind.ERROR) {
                        result.add(pd);
                    }
                }
            }
        }
        return result;
    }

    @Override
    public Collection<IErlModule> findAllIncludedFiles() throws CoreException {
        final List<IErlModule> checked = Lists.newArrayList();
        return findAllIncludedFiles(checked);
    }

    public Collection<IErlModule> findAllIncludedFiles(
            final List<IErlModule> checked) throws CoreException {
        final Collection<IErlModule> result = Sets.newHashSet();

        if (checked.contains(this)) {
            return result;
        }
        checked.add(this);

        final List<IErlModule> includedFilesForModule = ErlModel
                .getErlModelCache().getIncludedFilesForModule(this);
        if (includedFilesForModule != null && !includedFilesForModule.isEmpty()) {
            return includedFilesForModule;
        }
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
            final Collection<IErlModule> result,
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

    @Override
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

    @Override
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

    @Override
    public boolean exportsAllFunctions() {
        try {
            for (final IErlElement e : getChildrenOfKind(Kind.ATTRIBUTE)) {
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
        } catch (final ErlModelException e) { // ignore
        }
        return false;
    }

    @Override
    public boolean isRealFile() {
        return useCaches;
    }

    public String createScannerName() {
        final IResource res = getResource();
        if (res != null) {
            return createScannerNameFromResource(res);
        } else if (getFilePath() != null && isRealFile()) {
            return "mod" + getFilePath().hashCode() + "__" + getName();
        }
        // This is not used more than temporarily, so it's OK to have
        // a name that's temporary, as long as it's unique
        return "mod" + hashCode() + "_";
    }

    private String createScannerNameFromResource(final IResource res) {
        String resName;
        resName = "mod" + res.getFullPath().toPortableString().hashCode() + "_"
                + res.getName();
        return resName;
    }

    @Override
    public String getScannerName() {
        return scannerName;
    }

    @Override
    public IErlModule getModule() {
        return this;
    }

}
