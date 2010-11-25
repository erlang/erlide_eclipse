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
import org.erlide.core.erlang.ErlScanner;
import org.erlide.core.erlang.ErlToken;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlAttribute;
import org.erlide.core.erlang.IErlComment;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlExport;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlImport;
import org.erlide.core.erlang.IErlMember;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlPreprocessorDef;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IErlTypespec;
import org.erlide.core.erlang.IErlangFirstThat;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.core.erlang.ISourceReference;
import org.erlide.core.erlang.SourceRange;
import org.erlide.core.erlang.util.ErlangFunction;
import org.erlide.core.erlang.util.ErlangIncludeFile;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

public class ErlModule extends Openable implements IErlModule {

    private long timestamp = IResource.NULL_STAMP;
    private final List<IErlComment> comments = new ArrayList<IErlComment>(0);
    private String initialText;
    private ErlScanner scanner = null;
    private IFile fFile;
    private boolean parsed = false;
    private boolean updateCaches = true;

    private final ModuleKind moduleKind;
    protected final String path;

    protected ErlModule(final IErlElement parent, final String name,
            final String initialText, final IFile file, final String path) {
        super(parent, name);
        fFile = file;
        moduleKind = ErlideUtil.nameToModuleKind(name);
        this.initialText = initialText;
        if (ErlModelManager.verbose) {
            ErlLogger.debug("...creating " + parent.getName() + "/" + getName()
                    + " " + moduleKind);
        }
        this.path = path;
    }

    @Override
    protected synchronized boolean buildStructure(final IProgressMonitor pm)
            throws ErlModelException {
        final String path = getFilePath();
        if (scanner == null) {
            parsed = false;
        }
        final boolean initialParse = !parsed;
        if (scanner == null) {
            // There are two places that we make the initial scanner... this
            // is one
            getScanner();
        }
        parsed = ErlParser.parse(this, initialParse, path, updateCaches
                && useCaches());
        final IErlModel model = getModel();
        if (model != null) {
            model.notifyChange(this);
        }
        getScanner();
        // update timestamp (might be IResource.NULL_STAMP if original does not
        // exist)
        final IResource r = getResource();
        if (r instanceof IFile) {
            timestamp = ((IFile) r).getLocalTimeStamp();
        } else {
            timestamp = IResource.NULL_STAMP;
        }
        disposeScanner();
        return parsed;
    }

    protected boolean useCaches() {
        return fFile != null;
    }

    /**
     * @param underlyingResource
     * @return
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

    public void addMember(final IErlMember elem) {
        addChild(elem);
    }

    public void addComment(final IErlComment c) {
        comments.add(c);
    }

    public void removeChildren() {
        fChildren.clear();
        comments.clear();
    }

    public synchronized long getTimestamp() {
        return timestamp;
    }

    public Collection<IErlComment> getComments() {
        return comments;
    }

    public IErlImport findImport(final ErlangFunction function) {
        for (final IErlElement m : fChildren) {
            if (m instanceof IErlImport) {
                final IErlImport ei = (IErlImport) m;
                if (ei.hasFunction(function)) {
                    return ei;
                }
            }
        }
        return null;
    }

    public IErlExport findExport(final ErlangFunction function) {
        for (final IErlElement m : fChildren) {
            if (m instanceof IErlExport) {
                final IErlExport ei = (IErlExport) m;
                if (ei.hasFunction(function)) {
                    return ei;
                }
            }
        }
        return null;
    }

    public IErlFunction findFunction(final ErlangFunction function) {
        for (final IErlElement fun : fChildren) {
            if (fun instanceof IErlFunction) {
                final IErlFunction f = (IErlFunction) fun;
                if (f.getName().equals(function.name)
                        && (function.arity < 0 || f.getArity() == function.arity)) {
                    return f;
                }
            }
        }
        return null;
    }

    public IErlTypespec findTypespec(final String typeName) {
        for (final IErlElement child : fChildren) {
            if (child instanceof IErlTypespec) {
                final IErlTypespec typespec = (IErlTypespec) child;
                if (typespec.getName().equals(typeName)) {
                    return typespec;
                }
            }
        }
        return null;
    }

    public IErlPreprocessorDef findPreprocessorDef(final String definedName,
            final Kind type) {
        for (final IErlElement m : fChildren) {
            if (m instanceof IErlPreprocessorDef) {
                final IErlPreprocessorDef pd = (IErlPreprocessorDef) m;
                if (pd.getKind().equals(type)
                        && pd.getDefinedName().equals(definedName)) {
                    return pd;
                }
            }
        }
        return null;
    }

    public Collection<IErlPreprocessorDef> getPreprocessorDefs(final Kind type) {
        final List<IErlPreprocessorDef> res = new ArrayList<IErlPreprocessorDef>();
        for (final IErlElement m : fChildren) {
            if (m instanceof IErlPreprocessorDef) {
                final IErlPreprocessorDef pd = (IErlPreprocessorDef) m;
                if (pd.getKind().equals(type) || type.equals(Kind.ERROR)) {
                    res.add(pd);
                }
            }
        }
        return res;
    }

    public Collection<ErlangIncludeFile> getIncludedFiles()
            throws ErlModelException {
        if (!isStructureKnown()) {
            open(null);
        }
        final List<ErlangIncludeFile> r = new ArrayList<ErlangIncludeFile>(0);
        for (final IErlElement m : fChildren) {
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
        final List<IErlImport> r = new ArrayList<IErlImport>();
        for (final IErlElement m : fChildren) {
            if (m instanceof IErlImport) {
                r.add((IErlImport) m);
            }
        }
        return r;
    }

    public void getScanner() {
        if (scanner == null) {
            scanner = getNewScanner();
        }
        scanner.addRef();
    }

    private ErlScanner getNewScanner() {
        final String path = getFilePath();
        if (path == null) {
            return null;
        }
        return new ErlScanner(this, initialText, path);
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

    public String getModuleName() {
        return ErlideUtil.withoutExtension(getName());
    }

    public void disposeScanner() {
        if (scanner == null) {
            return;
        }
        final ErlScanner s = scanner;
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

    public IErlProject getProject() {
        for (IErlElement p = this; p != null; p = p.getParent()) {
            if (p instanceof IErlProject) {
                return (IErlProject) p;
            }
        }
        return null;
    }

    public Kind getKind() {
        return Kind.MODULE;
    }

    public void dispose() {
        disposeScanner();
        ErlangCore.getModelManager().removeModule(this);
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
        while (scanner != null) {
            disposeScanner();
        }
        initialText = newText;
        parsed = false;
        updateCaches = true;
        setStructureKnown(false);
        try {
            final boolean built = buildStructure(null);
            setStructureKnown(built);
        } catch (final ErlModelException e) {
            e.printStackTrace();
        }
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
}
