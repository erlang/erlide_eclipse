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
package org.erlide.engine.internal.model.erlang;

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
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.internal.model.root.Openable;
import org.erlide.engine.internal.util.ModelConfig;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.IParent;
import org.erlide.engine.model.erlang.ErlangFunction;
import org.erlide.engine.model.erlang.ErlangIncludeFile;
import org.erlide.engine.model.erlang.IErlAttribute;
import org.erlide.engine.model.erlang.IErlComment;
import org.erlide.engine.model.erlang.IErlExport;
import org.erlide.engine.model.erlang.IErlFunction;
import org.erlide.engine.model.erlang.IErlImport;
import org.erlide.engine.model.erlang.IErlMember;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.IErlPreprocessorDef;
import org.erlide.engine.model.erlang.IErlTypespec;
import org.erlide.engine.model.erlang.ISourceRange;
import org.erlide.engine.model.erlang.ISourceReference;
import org.erlide.engine.model.erlang.SourceKind;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlFolder;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.model.root.ISourceUnit;
import org.erlide.engine.services.parsing.ParserService;
import org.erlide.engine.services.parsing.ScannerService;
import org.erlide.engine.services.search.ModelUtilService;
import org.erlide.util.ErlLogger;
import org.erlide.util.SystemConfiguration;
import org.erlide.util.Util;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.google.common.base.Predicate;
import com.google.common.collect.Lists;

public class ErlModule extends Openable implements IErlModule {

    private static final OtpErlangAtom EXPORT_ALL = new OtpErlangAtom("export_all");
    private static final boolean logging = false;
    private IFile file;
    private final SourceKind moduleKind;
    protected String path;
    private String initialText;
    private boolean parsed;
    private final String scannerName;
    private final Collection<IErlComment> comments;
    private ScannerService scanner;
    private final String encoding;

    private final ModelUtilService modelUtilService;

    public ErlModule(final IParent parent, final String name, final IFile file) {
        this(parent, name, file, null, null, null);
    }

    public ErlModule(final IParent parent, final String name, final String path,
            final String encoding, final String initialText) {
        this(parent, name, null, path, encoding, initialText);
    }

    private ErlModule(final IParent parent, final String name, final IFile file,
            final String path, final String encoding, final String initialText) {
        super(parent, name);
        modelUtilService = ErlangEngine.getInstance().getModelUtilService();
        this.file = file;
        this.path = path;
        this.encoding = encoding;
        this.initialText = initialText;
        moduleKind = SourceKind.nameToModuleKind(name);
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
        setChildren(null);
        final String text = getInitialText();
        if (text != null) {
            final ParserService parser = ErlangEngine.getInstance().getParserService();
            parsed = parser.parse(this, scannerName, !parsed, getFilePath(), text, true);
            return parsed;
        }
        return true;
    }

    private String getInitialText() {
        String charset;
        if (initialText == null) {
            if (file != null) {
                if (file.isAccessible() && file.isSynchronized(0)) {
                    try {
                        charset = file.getCharset();
                        initialText = Util.getInputStreamAsString(file.getContents(),
                                charset);
                    } catch (final CoreException e) {
                        ErlLogger.warn(e);
                    }
                }
            } else if (path != null) {
                try {
                    if (encoding != null) {
                        charset = encoding;
                    } else {
                        charset = modelUtilService.getProject(this).getWorkspaceProject()
                                .getDefaultCharset();
                    }
                    initialText = Util.getInputStreamAsString(new FileInputStream(
                            new File(path)), charset);
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
    public synchronized boolean buildStructure(final IProgressMonitor pm)
            throws ErlModelException {
        if (internalBuildStructure(pm)) {
            final IErlModel model = ErlangEngine.getInstance().getModel();
            if (model != null) {
                model.notifyChange(this);
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
    public IErlElement getElementAt(final int position) throws ErlModelException {
        return ErlangEngine.getInstance().getModel()
                .innermostThat(this, new Predicate<IErlElement>() {
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
        return (IErlMember) ErlangEngine.getInstance().getModel()
                .innermostThat(this, new Predicate<IErlElement>() {
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
    public SourceKind getSourceKind() {
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

    public ISourceRange getSourceRange() {
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
    public IErlImport findImport(final ErlangFunction function) {
        try {
            for (final IErlElement e : getChildrenOfKind(ErlElementKind.IMPORT)) {
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
            for (final IErlElement e : getChildrenOfKind(ErlElementKind.EXPORT)) {
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
            for (final IErlElement fun : getChildrenOfKind(ErlElementKind.FUNCTION)) {
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
            for (final IErlElement child : getChildrenOfKind(ErlElementKind.TYPESPEC)) {
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
            final ErlElementKind kind) {
        synchronized (getModelLock()) {
            for (final IErlElement m : internalGetChildren()) {
                if (m instanceof IErlPreprocessorDef) {
                    final IErlPreprocessorDef pd = (IErlPreprocessorDef) m;
                    if (pd.getKind() == kind && pd.getDefinedName().equals(definedName)) {
                        return pd;
                    }
                }
            }
        }
        return null;
    }

    @Override
    public Collection<ErlangIncludeFile> getIncludeFiles() throws ErlModelException {
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
    public synchronized void reconcileText(final int offset, final int removeLength,
            final String newText, final IProgressMonitor mon) {
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
    public ErlElementKind getKind() {
        return ErlElementKind.MODULE;
    }

    @Override
    public void dispose() {
        if (scanner != null) {
            scanner.dispose();
            scanner = null;
        }
        ErlangEngine.getInstance().getModel().removeModule(this);
    }

    @Override
    public Set<ISourceUnit> getDirectDependentModules() throws ErlModelException {
        final Set<ISourceUnit> result = new HashSet<ISourceUnit>();
        final IErlProject project = modelUtilService.getProject(this);
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
    public Set<ISourceUnit> getAllDependentModules() throws CoreException {
        final Set<ISourceUnit> result = new HashSet<ISourceUnit>();
        final IErlProject project = modelUtilService.getProject(this);
        for (final IErlModule module : project.getModules()) {
            final Collection<IErlModule> allIncludedFiles = ErlangEngine.getInstance()
                    .getModelSearcherService().findAllIncludedFiles(module);
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
    public ScannerService getScanner() {
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

    private ScannerService getNewScanner() {
        final String filePath = getFilePath();
        final String text = getInitialText();
        scanner = ErlangEngine.getInstance().getScannerProviderService().get(scannerName);
        scanner.initialScan(text, filePath, logging);
        return scanner;
    }

    @Override
    public Collection<IErlPreprocessorDef> getPreprocessorDefs(final ErlElementKind kind) {
        final List<IErlPreprocessorDef> result = Lists.newArrayList();
        synchronized (getModelLock()) {
            for (final IErlElement e : internalGetChildren()) {
                if (e instanceof IErlPreprocessorDef) {
                    final IErlPreprocessorDef pd = (IErlPreprocessorDef) e;
                    if (pd.getKind() == kind || kind == ErlElementKind.PROBLEM) {
                        result.add(pd);
                    }
                }
            }
        }
        return result;
    }

    @Override
    public boolean isOnSourcePath() {
        final IParent parent = getParent();
        if (parent instanceof IErlFolder) {
            final IErlFolder folder = (IErlFolder) parent;
            return folder.isOnSourcePath();
        }
        if (checkPath(modelUtilService.getProject(this).getProperties().getSourceDirs())) {
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
        if (checkPath(modelUtilService.getProject(this).getProperties().getIncludeDirs())) {
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
            for (final IErlElement e : getChildrenOfKind(ErlElementKind.ATTRIBUTE)) {
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
        final IResource resource = getCorrespondingResource();
        if (resource != null) {
            return resource.getFullPath().toPortableString().substring(1);
        }
        int hash;
        if (initialText != null && !initialText.isEmpty()) {
            // we need to use the initialText if available, to avoid that
            // different anonymous modules get the same scanner name (which
            // causes problems in erlang compare)
            hash = initialText.hashCode();
        } else {
            hash = hashCode();
        }
        String name = getName();
        if (name == null) {
            name = "";
        }
        return String.format("%s_%08x", name, hash);
    }

    @Override
    public String getScannerName() {
        return scannerName;
    }

}
