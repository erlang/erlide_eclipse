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
package org.erlide.model.internal.erlang;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
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
import org.erlide.model.ErlModelException;
import org.erlide.model.IParent;
import org.erlide.model.erlang.IErlAttribute;
import org.erlide.model.erlang.IErlComment;
import org.erlide.model.erlang.IErlExport;
import org.erlide.model.erlang.IErlFunction;
import org.erlide.model.erlang.IErlImport;
import org.erlide.model.erlang.IErlMember;
import org.erlide.model.erlang.IErlModule;
import org.erlide.model.erlang.IErlParser;
import org.erlide.model.erlang.IErlPreprocessorDef;
import org.erlide.model.erlang.IErlScanner;
import org.erlide.model.erlang.IErlTypespec;
import org.erlide.model.erlang.ISourceRange;
import org.erlide.model.erlang.ISourceReference;
import org.erlide.model.erlang.ModuleKind;
import org.erlide.model.internal.root.ErlModel;
import org.erlide.model.internal.root.ModelConfig;
import org.erlide.model.internal.root.Openable;
import org.erlide.model.root.ErlModelManager;
import org.erlide.model.root.IErlElement;
import org.erlide.model.root.IErlExternal;
import org.erlide.model.root.IErlFolder;
import org.erlide.model.root.IErlModel;
import org.erlide.model.root.IErlProject;
import org.erlide.model.util.ErlangFunction;
import org.erlide.model.util.ErlangIncludeFile;
import org.erlide.model.util.ModelUtils;
import org.erlide.util.ErlLogger;
import org.erlide.util.SystemConfiguration;
import org.erlide.util.Util;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.google.common.base.Predicate;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

public class ErlModule extends Openable implements IErlModule {

    private static final OtpErlangAtom EXPORT_ALL = new OtpErlangAtom(
            "export_all");
    private static final boolean logging = false;
    private long timestamp = IResource.NULL_STAMP;
    private IFile file;
    private final ModuleKind moduleKind;
    protected String path;
    private String initialText;
    private boolean parsed;
    private final String scannerName;
    private final Collection<IErlComment> comments;
    private IErlScanner scanner;
    private final String encoding;

    public ErlModule(final IParent parent, final String name, final IFile file) {
        this(parent, name, file, null, null, null);
    }

    public ErlModule(final IParent parent, final String name,
            final String path, final String encoding, final String initialText) {
        this(parent, name, null, path, encoding, initialText);
    }

    private ErlModule(final IParent parent, final String name,
            final IFile file, final String path, final String encoding,
            final String initialText) {
        super(parent, name);
        this.file = file;
        this.path = path;
        this.encoding = encoding;
        this.initialText = initialText;
        moduleKind = ModuleKind.nameToModuleKind(name);
        parsed = false;
        scannerName = createScannerName();
        comments = Lists.newArrayList();
        if (ModelConfig.verbose) {
            final IErlElement element = (IErlElement) parent;
            final String parentName = element.getName();
            ErlLogger.debug("...creating " + parentName + "/" + getName() + " "
                    + moduleKind);
        }
    }

    public boolean internalBuildStructure(final IProgressMonitor pm) {
        final String text = getInitialText();
        if (text != null) {
            final IErlParser parser = ErlModelManager.getErlangModel()
                    .getParser();
            parsed = parser.parse(this, scannerName, !parsed, getFilePath(),
                    text, true);
            return parsed;
        } else {
            setChildren(null);
            return true;
        }
    }

    private String getInitialText() {
        String charset;
        if (initialText == null) {
            if (file != null) {
                if (file.isAccessible() && file.isSynchronized(0)) {
                    try {
                        charset = file.getCharset();
                        initialText = Util.getInputStreamAsString(
                                file.getContents(), charset);
                    } catch (final CoreException e) {
                        ErlLogger.warn(e);
                    }
                }
            } else if (path != null) {
                try {
                    if (encoding != null) {
                        charset = encoding;
                    } else {
                        charset = ModelUtils.getProject(this)
                                .getWorkspaceProject().getDefaultCharset();
                    }
                    initialText = Util.getInputStreamAsString(
                            new FileInputStream(new File(path)), charset);
                } catch (final CoreException e) {
                    ErlLogger.warn(e);
                } catch (final FileNotFoundException e) {
                    ErlLogger.warn(e);
                }
            }
        }
        return initialText;
    }

    @Override
    protected synchronized boolean buildStructure(final IProgressMonitor pm)
            throws ErlModelException {
        if (internalBuildStructure(pm)) {
            final IErlModel model = ErlModelManager.getErlangModel();
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
        if (file != null) {
            final IPath location = file.getLocation();
            if (location != null) {
                return location.toString();
            }
        }
        return path;
    }

    @Override
    public IErlElement getElementAt(final int position)
            throws ErlModelException {
        return ErlModelManager.getErlangModel().innermostThat(this,
                new Predicate<IErlElement>() {
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
        return (IErlMember) ErlModelManager.getErlangModel().innermostThat(
                this, new Predicate<IErlElement>() {
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
        return file;
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
        if (scanner != null) {
            scanner.replaceText(offset, removeLength, newText);
        }
        if (mon != null) {
            mon.worked(1);
        }
        setStructureKnown(false);
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
        return SystemConfiguration.withoutExtension(getName());
    }

    @Override
    public Kind getKind() {
        return Kind.MODULE;
    }

    @Override
    public void dispose() {
        if (scanner != null) {
            scanner.dispose();
            scanner = null;
        }
        ErlModelManager.getErlangModel().removeModule(this);
    }

    @Override
    public Set<IErlModule> getDirectDependentModules() throws ErlModelException {
        final Set<IErlModule> result = new HashSet<IErlModule>();
        final IErlProject project = ModelUtils.getProject(this);
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
        final IErlProject project = ModelUtils.getProject(this);
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
        initialText = newText;
        parsed = false;
        setStructureKnown(false);
        scanner.initialScan(newText, "", logging);
        final boolean built = buildStructure(null);
        setStructureKnown(built);
    }

    @Override
    public void setResource(final IFile file) {
        this.file = file;
    }

    @Override
    public String getLabelString() {
        return getName();
    }

    @Override
    public IErlScanner getScanner() {
        if (scanner == null) {
            scanner = getNewScanner();
        } else {
            scanner.addref();
        }
        return scanner;
    }

    @Override
    public void createScanner() {
        if (scanner != null) {
            scanner.dispose();
        }
        scanner = getNewScanner();
    }

    private IErlScanner getNewScanner() {
        final String filePath = getFilePath();
        final String text = getInitialText();
        return ErlModelManager.getErlangModel().getToolkit()
                .createScanner(scannerName, text, filePath, logging);
    }

    @Override
    public Collection<IErlPreprocessorDef> getPreprocessorDefs(final Kind kind) {
        final List<IErlPreprocessorDef> result = Lists.newArrayList();
        synchronized (getModelLock()) {
            for (final IErlElement e : internalGetChildren()) {
                if (e instanceof IErlPreprocessorDef) {
                    final IErlPreprocessorDef pd = (IErlPreprocessorDef) e;
                    if (pd.getKind() == kind || kind == Kind.PROBLEM) {
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
        final IErlProject project = ModelUtils.getProject(this);
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
        final Collection<IErlProject> projects = ErlModelManager
                .getErlangModel().getErlangProjects();
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
        if (checkPath(ModelUtils.getProject(this).getSourceDirs())) {
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
        if (checkPath(ModelUtils.getProject(this).getIncludeDirs())) {
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

    public String createScannerName() {
        final IResource res = getResource();
        if (res != null) {
            return createScannerNameFromResource(res);
        } else if (getFilePath() != null) {
            return "mod" + getFilePath().hashCode() + "__" + getName();
        } else if (getName() != null) {
            return "mod" + hashCode() + "_" + getName();
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
    public boolean getLogging() {
        return logging;
    }

}
