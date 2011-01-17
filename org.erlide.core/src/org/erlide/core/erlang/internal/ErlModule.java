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
package org.erlide.core.erlang.internal;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlToken;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlAttribute;
import org.erlide.core.erlang.IErlComment;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlExport;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlImport;
import org.erlide.core.erlang.IErlModelMap;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlModuleInternal;
import org.erlide.core.erlang.IErlPreprocessorDef;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IErlTypespec;
import org.erlide.core.erlang.IErlangFirstThat;
import org.erlide.core.erlang.IParent;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.core.erlang.ISourceReference;
import org.erlide.core.erlang.SourceRange;
import org.erlide.core.erlang.util.ErlangFunction;
import org.erlide.core.erlang.util.ErlangIncludeFile;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.google.common.collect.Lists;

public class ErlModule extends Openable implements IErlModule {

    private long timestamp = IResource.NULL_STAMP;
    private IFile fFile;
    private final ModuleKind moduleKind;
    protected final String path;
    private final String initialText;

    protected ErlModule(final IParent parent, final String name,
            final String initialText, final IFile file, final String path) {
        super(parent, name);
        this.initialText = initialText;
        fFile = file;
        moduleKind = ErlideUtil.nameToModuleKind(name);
        this.path = path;
        if (ErlModelManager.verbose) {
            final IErlElement element = (IErlElement) parent;
            final String parentName = element.getName();
            ErlLogger.debug("...creating " + parentName + "/" + getName() + " "
                    + moduleKind);
        }
        final IErlModelMap erlModelMap = ErlangCore.getModelMap();
        erlModelMap.putModule(this);
    }

    @Override
    protected synchronized boolean buildStructure(final IProgressMonitor pm)
            throws ErlModelException {
        if (getModuleInternal().buildStructure(pm)) {
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

    /*
     * (non-Javadoc)
     * 
     * @see org.erlide.core.erlang.internal.ErlElement#getFilePath()
     */
    @Override
    public String getFilePath() {
        if (path != null) {
            return path;
        }
        final IPath location = fFile.getLocation();
        if (location == null) {
            return null;
        }
        return location.toString();
    }

    public IErlElement getElementAt(final int position)
            throws ErlModelException {
        return getModel().innermostThat(this, new IErlangFirstThat() {
            public boolean firstThat(final IErlElement e) {
                try {
                    if (e instanceof ISourceReference) {
                        final ISourceReference ch = (ISourceReference) e;
                        ISourceRange r;
                        r = ch.getSourceRange();
                        if (r != null && r.hasPosition(position)) {
                            return true;
                        }
                    }
                } catch (final ErlModelException e1) {
                    ErlLogger.error(e1);
                }
                return false;
            }
        });
    }

    public IErlElement getElementAtLine(final int lineNumber) {
        return getModel().innermostThat(this, new IErlangFirstThat() {
            public boolean firstThat(final IErlElement e) {
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

    public void copy(final IErlElement container, final IErlElement sibling,
            final String rename, final boolean replace,
            final IProgressMonitor monitor) throws ErlModelException {
        // TODO Auto-generated method stub

    }

    public void delete(final boolean force, final IProgressMonitor monitor)
            throws ErlModelException {
        // TODO Auto-generated method stub

    }

    public void move(final IErlElement container, final IErlElement sibling,
            final String rename, final boolean replace,
            final IProgressMonitor monitor) throws ErlModelException {
        // TODO Auto-generated method stub

    }

    public void rename(final String aname, final boolean replace,
            final IProgressMonitor monitor) throws ErlModelException {
        // TODO Auto-generated method stub

    }

    @Override
    protected boolean hasBuffer() {
        return true;
    }

    @Override
    public void addChild(final IErlElement child) {
        getModuleInternal().addChild(child);
    }

    private IErlModuleInternal getModuleInternal() {
        return ErlModelMap.getDefault().get(this);
    }

    @Override
    public int getChildCount() {
        return getModuleInternal().getChildCount();
    }

    @Override
    public List<IErlElement> getChildren() throws ErlModelException {
        return getModuleInternal().getChildren();
    }

    @Override
    public IErlElement getChildNamed(final String name) {
        return getModuleInternal().getChildNamed(name);
    }

    @Override
    public List<IErlElement> getChildrenOfKind(final Kind kind)
            throws ErlModelException {
        return getModuleInternal().getChildrenOfKind(kind);
    }

    public void addComment(final IErlComment c) {
        getModuleInternal().addComment(c);
    }

    public void removeChildren() {
        getModuleInternal().removeChildren();
    }

    public synchronized long getTimestamp() {
        return timestamp;
    }

    public Collection<IErlComment> getComments() {
        return getModuleInternal().getComments();
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

    public Collection<ErlangIncludeFile> getIncludedFiles()
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
        getModuleInternal().reconcileText(offset, removeLength, newText, mon);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.erlide.core.erlang.IErlModule#postReconcile(org.eclipse.core.runtime
     * .IProgressMonitor)
     */
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
    protected void closing(final Object info) throws ErlModelException {
        // TODO Auto-generated method stub

    }

    public synchronized void initialReconcile() {
        // currently unused
        // Note that the ErlReconciler doesn't send the first full-text
        // reconcile that the built-in reconciler does
    }

    public synchronized void finalReconcile() {
        // currently unused
    }

    @Override
    public String getModuleName() {
        return ErlideUtil.withoutExtension(getName());
    }

    public void disposeScanner() {
        getModuleInternal().disposeScanner();
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

    public void dispose() {
        disposeScanner();
        ErlangCore.getModelManager().removeModule(this);
        ErlangCore.getModelMap().removeModule(this);
    }

    public Set<IErlModule> getDirectDependents() throws ErlModelException {
        final Set<IErlModule> result = new HashSet<IErlModule>();
        final IErlProject project = getProject();
        for (final IErlModule m : project.getModules()) {
            ErlLogger.debug(m.toString());
            final boolean wasOpen = m.isOpen();
            if (!wasOpen) {
                m.open(null);
            }
            final Collection<ErlangIncludeFile> incs = m.getIncludedFiles();
            for (final ErlangIncludeFile inc : incs) {
                if (inc.getFilename().equals(getName())) {
                    result.add(m);
                    break;
                }
            }
            if (!wasOpen) {
                m.close();
            }
        }
        return result;
    }

    public Set<IErlModule> getAllDependents() throws ErlModelException {
        final Set<IErlModule> mod = getDirectDependents();
        return getAllDependents(mod, new HashSet<IErlModule>());
    }

    private Set<IErlModule> getAllDependents(final Set<IErlModule> current,
            final Set<IErlModule> result) throws ErlModelException {
        final Set<IErlModule> next = new HashSet<IErlModule>();
        for (final IErlModule mod : current) {
            final Collection<IErlModule> dep = mod.getDirectDependents();
            result.add(mod);
            next.addAll(dep);
        }
        if (next.size() == 0) {
            return result;

        } else {
            return getAllDependents(next, result);
        }
    }

    public synchronized void resetAndCacheScannerAndParser(final String newText) {
        getModuleInternal().resetAndCacheScannerAndParser(newText);
    }

    public ErlToken getScannerTokenAt(final int offset) {
        return getModuleInternal().getScannerTokenAt(offset);
    }

    public void setResource(final IFile file) {
        fFile = file;
    }

    @Override
    public String getLabelString() {
        return getName();
    }

    public void getScanner() {
        getModuleInternal().getScanner();
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

    @Override
    public boolean isStructureKnown() {
        return getModuleInternal().isStructureKnown();
    }

    @Override
    public void setStructureKnown(final boolean newStructureKnown) {
        getModuleInternal().setStructureKnown(newStructureKnown);
    }

    public IErlProject getProject() {
        final IErlElement ancestor = getAncestorOfKind(Kind.PROJECT);
        if (ancestor instanceof IErlProject) {
            return (IErlProject) ancestor;
        }
        return null;
    }

    public String getInitialText() {
        return initialText;
    }

}
