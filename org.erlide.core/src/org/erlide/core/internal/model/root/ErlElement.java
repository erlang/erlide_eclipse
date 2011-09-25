/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.core.internal.model.root;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.PlatformObject;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.erlide.core.CoreScope;
import org.erlide.core.common.Util;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlModelStatus;
import org.erlide.core.model.root.ErlModelStatusConstants;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlElementVisitor;
import org.erlide.core.model.root.IErlModel;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.model.root.IOpenable;
import org.erlide.core.model.root.IParent;
import org.erlide.jinterface.ErlLogger;

import com.google.common.base.Objects;
import com.google.common.collect.Lists;

/**
 * Root of Erlang element handle hierarchy.
 * 
 * @see IErlElement
 */
public abstract class ErlElement extends PlatformObject implements IErlElement,
        IParent, Cloneable {

    /**
     * This element's parent, or <code>null</code> if this element does not have
     * a parent.
     */
    private final IParent fParent;

    private final List<IErlElement> fChildren = Lists.newArrayList();

    /**
     * This element's name, or an empty <code>String</code> if this element does
     * not have a name.
     */
    protected String fName;

    protected static final Object NO_INFO = new Object();

    /**
     * Constructs a handle for a Erlang element with the given parent element
     * and name.
     * 
     * @param parent
     *            The parent of Erlang element
     * @param name
     *            The name of Erlang element
     * 
     * @throws IllegalArgumentException
     *             if the type is not one of the valid Erlang element type
     *             constants
     * 
     */
    protected ErlElement(final IParent parent, final String name) {
        fParent = parent;
        fName = name;
        Assert.isNotNull(fName);
    }

    /**
     * @see IOpenable
     */
    public void close() throws ErlModelException {
        // /ErlModelManager.getErlangModelManager().removeInfoAndChildren(this);
    }

    /**
     * This element is being closed. Do any necessary cleanup.
     */
    protected void closing(final Object info) throws ErlModelException {
    }

    /**
     * Returns true if this handle represents the same Erlang element as the
     * given handle. By default, two handles represent the same element if they
     * are identical or if they represent the same type of element, have equal
     * names, parents, and occurrence counts.
     * 
     * <p>
     * If a subclass has other requirements for equality, this method must be
     * overridden.
     * 
     * @see Object#equals
     */
    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null) {
            return false;
        }

        // Erlang model parent is null
        if (fParent == null) {
            return super.equals(o);
        }

        if (o instanceof ErlElement) { // WHY OH WHY?!?!?!? This was a tough
            // bug (jc)
            // assume instanceof check is done in subclass
            final ErlElement other = (ErlElement) o;
            return fName.equals(other.fName) && fParent.equals(other.fParent);
        }
        return false;
    }

    /**
     * @see IErlElement
     */
    public boolean exists() {
        return true;
    }

    /**
     * @see IErlElement
     */
    public IErlElement getAncestorOfKind(final Kind kind) {
        IErlElement element = this;
        while (true) {
            if (element.getKind() == kind) {
                return element;
            }
            final IParent parent = element.getParent();
            if (parent instanceof IErlElement) {
                element = (IErlElement) parent;
            } else {
                break;
            }
        }
        return null;
    }

    /**
     * @see IErlElement
     */
    public String getName() {
        return fName;
    }

    /**
     * @see IErlElement
     */
    public IErlModel getModel() {
        return CoreScope.getModel();
    }

    /**
     * @see IErlElement
     */
    public IErlProject getProject() {
        final IErlElement ancestor = getAncestorOfKind(Kind.PROJECT);
        if (ancestor instanceof IErlProject) {
            return (IErlProject) ancestor;
        }
        return null;
    }

    /**
     * Return the first instance of IOpenable in the parent hierarchy of this
     * element.
     * 
     * <p>
     * Subclasses that are not IOpenable's must override this method.
     */
    public IOpenable getOpenableParent() {
        return (IOpenable) fParent;
    }

    /**
     * @see IErlElement
     */
    public IParent getParent() {
        return fParent;
    }

    static class NoResourceSchedulingRule implements ISchedulingRule {

        public IPath fPath;

        public NoResourceSchedulingRule(final IPath path) {
            fPath = path;
        }

        public boolean contains(final ISchedulingRule rule) {
            if (rule instanceof NoResourceSchedulingRule) {
                return fPath
                        .isPrefixOf(((NoResourceSchedulingRule) rule).fPath);
            }
            return false;
        }

        public boolean isConflicting(final ISchedulingRule rule) {
            if (rule instanceof NoResourceSchedulingRule) {
                final IPath otherPath = ((NoResourceSchedulingRule) rule).fPath;
                return fPath.isPrefixOf(otherPath)
                        || otherPath.isPrefixOf(fPath);
            }
            return false;
        }
    }

    public ISchedulingRule getSchedulingRule() {
        final IResource resource = getResource();
        if (resource == null) {
            return new NoResourceSchedulingRule(new Path(getName()));
        }
        return resource;
    }

    /**
     * @see IParent
     */
    public boolean hasChildren() {
        try {
            return !getChildren().isEmpty();
        } catch (final ErlModelException e) {
            return false;
        }
    }

    public boolean hasChildrenOfKind(final Kind kind) {
        try {
            for (final IErlElement child : getChildren()) {
                if (child.getKind() == kind) {
                    return true;
                }
            }
        } catch (final ErlModelException e) {
            // ignore
        }
        return false;
    }

    /**
     * Returns the hash code for this Erlang element. By default, the hash code
     * for an element is a combination of its name and parent's hash code.
     * Elements with other requirements must override this method.
     */
    @Override
    public int hashCode() {
        if (fParent == null) {
            return super.hashCode();
        }
        return Objects.hashCode(fName, fParent);
    }

    /**
     * @see IErlElement
     */
    public boolean isReadOnly() {
        return false;
    }

    /**
     * Creates and returns and not present exception for this element.
     */
    protected ErlModelException newNotPresentException() {
        ErlLogger.debug("not found: " + fName);
        return new ErlModelException(new ErlModelStatus(
                ErlModelStatusConstants.ELEMENT_DOES_NOT_EXIST, this));
    }

    /**
     */
    public String readableName() {
        return getName();
    }

    protected String tabString(final int tab) {
        return " ";
        // final StringBuilder buffer = new StringBuilder();
        // for (int i = tab; i > 0; i--) {
        // buffer.append(" "); //$NON-NLS-1$
        // }
        // return buffer.toString();
    }

    /**
     * Debugging purposes
     */
    public String toDebugString() {
        final StringBuilder buffer = new StringBuilder();
        this.toStringInfo(0, buffer, NO_INFO);
        return buffer.toString();
    }

    @Override
    public String toString() {
        final StringBuilder buffer = new StringBuilder();
        toString(0, buffer);
        return buffer.toString();
    }

    /**
     * Debugging purposes
     */
    protected void toString(final int tab, final StringBuilder buffer) {
        final Object info = this.toStringInfo(tab, buffer);
        if (tab == 0) {
            toStringAncestors(buffer);
        }
        toStringChildren(tab, buffer, info);
    }

    /**
     * Debugging purposes
     */
    public String toStringWithAncestors() {
        final StringBuilder buffer = new StringBuilder();
        this.toStringInfo(0, buffer, NO_INFO);
        toStringAncestors(buffer);
        return buffer.toString();
    }

    /**
     * Debugging purposes
     */
    protected void toStringAncestors(final StringBuilder buffer) {
        final IParent parent = getParent();
        if (parent != null) {
            if (parent instanceof ErlElement) {
                final ErlElement parentElement = (ErlElement) parent;
                buffer.append("[> "); //$NON-NLS-1$
                parentElement.toStringInfo(0, buffer, NO_INFO);
                parentElement.toStringAncestors(buffer);
            }
            buffer.append("] "); //$NON-NLS-1$
        }
    }

    /**
     * Debugging purposes
     */
    protected void toStringChildren(final int tab, final StringBuilder buffer,
            final Object info) {
        if (info == null || !(info instanceof ErlElement)) {
            return;
        }
        if (getChildCount() > 0) {
            buffer.append("{");
            int i = 0;
            try {
                for (final IErlElement element : getChildren()) {
                    ((ErlElement) element).toString(tab + 1, buffer);
                    buffer.append(","); //$NON-NLS-1$
                    if (++i > 3) {
                        buffer.append("...");
                        break;
                    }
                }
            } catch (final ErlModelException e) {
                // ignore
            }
            buffer.deleteCharAt(buffer.length() - 1);
            buffer.append("}");
        }
    }

    /**
     * Debugging purposes
     */
    public Object toStringInfo(final int tab, final StringBuilder buffer) {
        this.toStringInfo(tab, buffer, this);
        return this;
    }

    /**
     * Debugging purposes
     */
    protected void toStringInfo(final int tab, final StringBuilder buffer,
            final Object info) {
        buffer.append(tabString(tab));
        buffer.append(getName());
        if (info == null) {
            buffer.append(" (not open)"); //$NON-NLS-1$
        }
    }

    /**
     * Is the structure of this element known
     * 
     * @see IErlElement#isStructureKnown()
     */
    protected boolean structureKnown = false;

    public void clearCaches() {
    }

    @Override
    public Object clone() {
        try {
            return super.clone();
        } catch (final CloneNotSupportedException e) {
            throw new Error();
        }
    }

    public List<IErlElement> getChildren() throws ErlModelException {
        return Collections.unmodifiableList(Lists.newArrayList(fChildren));
    }

    public int getChildCount() {
        try {
            return getChildren().size();
        } catch (final ErlModelException e) {
            return 0;
        }
    }

    /**
     * Returns a collection of (immediate) children of this node of the
     * specified type.
     * 
     * @param type
     *            - one of the constants defined by IErlElement
     */
    public List<IErlElement> getChildrenOfKind(final Kind kind)
            throws ErlModelException {
        final List<IErlElement> result = Lists.newArrayList();
        for (final IErlElement element : getChildren()) {
            if (element.getKind() == kind) {
                result.add(element);
            }
        }
        return result;
    }

    public IErlElement getChildNamed(final String name) {
        return getChildNamed(this, name);
    }

    public IErlElement getChildWithResource(final IResource rsrc) {
        return getChildWithResource(this, rsrc);
    }

    /**
     * Returns <code>true</code> if this child is in my children collection
     */
    protected boolean includesChild(final IErlElement child) {
        try {
            return getChildren().contains(child);
        } catch (final ErlModelException e) {
            return false;
        }
    }

    /**
     * @see IErlElement#isStructureKnown()
     */
    public boolean isStructureKnown() {
        return structureKnown;
    }

    public void removeChild(final IErlElement child) {
        clearCaches();
        fChildren.remove(child);
    }

    public void addChild(final IErlElement child) {
        clearCaches();
        fChildren.add(child);
    }

    public void setChildren(final Collection<? extends IErlElement> children) {
        clearCaches();
        fChildren.clear();
        if (children != null) {
            fChildren.addAll(children);
        }
    }

    /**
     * Sets whether the structure of this element known
     * 
     * @see IErlElement#isStructureKnown()
     */
    public void setStructureKnown(final boolean newStructureKnown) {
        if (structureKnown != newStructureKnown) {
            structureKnown = newStructureKnown;
        }
    }

    public void resourceChanged(final IResourceDelta delta) {
        // FIXME is this enough? it will rebuild at next occasion, and modules
        // are handled with reconciles, containers children through add and
        // remove, but... e.g. name change of folder?
        setStructureKnown(false);
    }

    private static IErlElement getChildNamed(final ErlElement parent,
            final String name) {
        try {
            for (final IErlElement child : parent.getChildren()) {
                if (child.getName().equals(name)) {
                    return child;
                }
            }
        } catch (final ErlModelException e) {
            // ignore
        }
        return null;
    }

    private static IErlElement getChildWithResource(final ErlElement parent,
            final IResource rsrc) {
        try {
            for (final IErlElement child : parent.getChildren()) {
                if (rsrc.equals(child.getResource())) {
                    return child;
                }
            }
        } catch (final ErlModelException e) {
            // ignore
        }
        return null;
    }

    public final void accept(final IErlElementVisitor visitor,
            final Set<AcceptFlags> flags, final IErlElement.Kind leafKind)
            throws ErlModelException {
        if (getKind() == leafKind) {
            visitor.visit(this);
        } else {
            boolean visitChildren = true;
            if (!flags.contains(AcceptFlags.LEAFS_ONLY)
                    && !flags.contains(AcceptFlags.CHILDREN_FIRST)) {
                visitChildren = visitor.visit(this);
            }
            if (visitChildren) {
                for (final IErlElement child : getChildren()) {
                    child.accept(visitor, flags, leafKind);
                }
            }
            if (!flags.contains(AcceptFlags.LEAFS_ONLY)
                    && flags.contains(AcceptFlags.CHILDREN_FIRST)) {
                visitor.visit(this);
            }
        }
    }

    /**
     * Return my corresponding resource. Overridden in IErlModule, IErlFolder
     * and IErlProject
     */
    public IResource getCorrespondingResource() {
        return null;
    }

    public IResource getResource() {
        if (fParent instanceof IErlElement) {
            final IErlElement parentElement = (IErlElement) fParent;
            return parentElement.getResource();
        }
        return null;
    }

    public String getLabelString() {
        return Util.normalizeSpaces(toString());
    }

    public String getFilePath() {
        return null;
    }

    public void dispose() {
        // XXX FIXME TODO
        // if (!LOCAL_CHILDREN) {
        // getModel().setChildrenOf(this, null);
        // }
    }

    protected ErlModelCache getModelCache() {
        return ErlModelCache.getDefault();
    }

}
