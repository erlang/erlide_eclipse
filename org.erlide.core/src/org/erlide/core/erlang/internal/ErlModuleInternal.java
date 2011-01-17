package org.erlide.core.erlang.internal;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlScanner;
import org.erlide.core.erlang.ErlToken;
import org.erlide.core.erlang.IErlComment;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlElement.Kind;
import org.erlide.core.erlang.IErlModuleInternal;

import com.google.common.collect.Lists;

public class ErlModuleInternal implements IErlModuleInternal {

    private final String path;
    private final String scannerName;
    String initialText;
    private final List<IErlElement> children;
    private final List<IErlComment> comments;
    private ErlScanner scanner;
    private boolean parsed;// FIXME is both this and structureKnown needed?
    private boolean structureKnown;
    private boolean updateCaches;

    public ErlModuleInternal(final String path, final String scannerName,
            final String initialText) {
        this.path = path;
        this.scannerName = scannerName;
        this.initialText = initialText;
        children = Lists.newArrayList();
        comments = Lists.newArrayList();
        scanner = null;
        parsed = false;
        structureKnown = false;
        updateCaches = false;
    }

    public void addChild(final IErlElement child) {
        children.add(child);
    }

    public int getChildCount() {
        return children.size();
    }

    public List<IErlElement> getChildren() throws ErlModelException {
        return children; // FIXME should we make a shallow or immutable copy?
    }

    public boolean hasChildren() {
        return !children.isEmpty();
    }

    public List<IErlElement> getChildrenOfKind(final Kind kind)
            throws ErlModelException {
        final List<IErlElement> result = Lists.newArrayList();
        for (final IErlElement e : children) {
            if (e.getKind() == kind) {
                result.add(e);
            }
        }
        return result;
    }

    public boolean hasChildrenOfKind(final Kind kind) {
        for (final IErlElement e : children) {
            if (e.getKind() == kind) {
                return true;
            }
        }
        return false;
    }

    public void removeChild(final IErlElement e) {
        children.remove(e);
    }

    public IErlElement getChildNamed(final String s) {
        for (final IErlElement e : children) {
            if (e.getName().equals(s)) {
                return e;
            }
        }
        return null;
    }

    public IErlElement getChildWithResource(final IResource rsrc) {
        for (final IErlElement e : children) {
            final IResource r = e.getResource();
            if (r != null && r.equals(rsrc)) {
                return e;
            }
        }
        return null;
    }

    public void removeChildren() {
        children.clear();
        comments.clear();
    }

    public void addComment(final IErlComment c) {
        comments.add(c);
    }

    public Collection<IErlComment> getComments() {
        return comments;
    }

    public boolean buildStructure(final IProgressMonitor pm) {
        if (scanner == null) {
            parsed = false;
        }
        final boolean initialParse = !parsed;
        if (scanner == null) {
            // There are two places that we make the initial scanner... this
            // is one
            getScanner();
        }
        parsed = ErlParser.parse(this, scannerName, initialParse, path,
                updateCaches);
        // XXX
        // TODO
        // FIXME
        // Should we notify change somehow? On all modules represented by this?
        // How!?
        // if (model != null) {
        // model.notifyChange(this);
        // }
        return parsed;
    }

    public void getScanner() {
        if (scanner == null) {
            scanner = getNewScanner();
        }
        scanner.addRef();
    }

    private ErlScanner getNewScanner() {
        if (path == null) {
            return null;
        }
        return new ErlScanner(this, scannerName, initialText, path);
    }

    public void reconcileText(final int offset, final int removeLength,
            final String newText, final IProgressMonitor mon) {

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

    public void setStructureKnown(final boolean b) {
        structureKnown = b;
    }

    public void resetAndCacheScannerAndParser(final String newText) {
        while (scanner != null) {
            disposeScanner();
        }
        initialText = newText;
        parsed = false;
        updateCaches = true;
        setStructureKnown(false);
        final boolean built = buildStructure(null);
        setStructureKnown(built);
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

    public boolean isStructureKnown() {
        return structureKnown;
    }

    public ErlToken getScannerTokenAt(final int offset) {
        if (scanner != null) {
            return scanner.getTokenAt(offset);
        }
        return null;
    }

    public String getPath() {
        return path;
    }

}
