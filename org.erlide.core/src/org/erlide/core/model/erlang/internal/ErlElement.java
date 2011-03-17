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
package org.erlide.core.model.erlang.internal;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.PlatformObject;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.erlide.core.ErlangCore;
import org.erlide.core.common.Util;
import org.erlide.core.model.erlang.ErlModelException;
import org.erlide.core.model.erlang.ErlModelStatusConstants;
import org.erlide.core.model.erlang.IErlElement;
import org.erlide.core.model.erlang.IErlElementVisitor;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.IErlProject;
import org.erlide.core.model.erlang.IOpenable;
import org.erlide.core.model.erlang.IParent;
import org.erlide.core.model.erlang.ISourceRange;
import org.erlide.core.model.erlang.ISourceReference;
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

    public static final char EM_ESCAPE = '\\';

    public static final char EM_PROJECT = '=';

    public static final char EM_PACKAGE = '/';

    public static final char EM_MODULE = '{';

    public static final char EM_ATTRIBUTE = '<';

    public static final char EM_FUNCTION = '^';

    public static final char EM_LIBRARY = '~';

    public static final char EM_BEAMFILE = '|';

    public static final char EM_COUNT = '!';

    /**
     * A count to uniquely identify this element in the case that a duplicate
     * named element exists. For example, if there are two fields in a
     * compilation unit with the same name, the occurrence count is used to
     * distinguish them. The occurrence count starts at 1 (thus the first
     * occurrence is occurrence 1, not occurrence 0).
     */
    public int fOccurrenceCount = 1;

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

    public static final ErlElement[] NO_ELEMENTS = new ErlElement[0];

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
    protected abstract void closing(Object info) throws ErlModelException;

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
            return fOccurrenceCount == other.fOccurrenceCount
                    && fName.equals(other.fName)
                    && fParent.equals(other.fParent);
        }
        return false;
    }

    protected void escapeMementoName(final StringBuilder buffer,
            final String mementoName) {
        for (int i = 0, length = mementoName.length(); i < length; i++) {
            final char character = mementoName.charAt(i);
            switch (character) {
            case EM_ESCAPE:
            case EM_COUNT:
            case EM_PROJECT:
            case EM_PACKAGE:
            case EM_MODULE:
            case EM_ATTRIBUTE:
            case EM_FUNCTION:
            case EM_LIBRARY:
            case EM_BEAMFILE:
                buffer.append(EM_ESCAPE);
            }
            buffer.append(character);
        }
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
    public ErlModel getModel() {
        return (ErlModel) ErlangCore.getModel();
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

    public IErlModule getModule() {
        final IErlElement ancestor = getAncestorOfKind(Kind.MODULE);
        if (ancestor instanceof IErlModule) {
            return (IErlModule) ancestor;
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

    /**
     * Returns the element that is located at the given source position in this
     * element. This is a helper method for <code>IErlModule#getElementAt</code>
     * , and only works on compilation units and types. The position given is
     * known to be within this element's source range already, and if no finer
     * grained element is found at the position, this element is returned.
     */
    protected IErlElement getSourceElementAt(final int position)
            throws ErlModelException {
        if (this instanceof ISourceReference) {
            for (final IErlElement i : internalGetChildren()) {
                if (i instanceof SourceRefElement) {
                    final SourceRefElement child = (SourceRefElement) i;
                    final ISourceRange range = child.getSourceRange();
                    final int start = range.getOffset();
                    final int end = start + range.getLength();
                    if (start <= position && position <= end) {
                        return child.getSourceElementAt(position);
                    }
                }
            }
        } else {
            // should not happen
            Assert.isTrue(false);
        }
        return this;
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

    /*
     * (non-Edoc)
     * 
     * @see org.erlide.core.model.erlang.IErlElement#getSchedulingRule()
     */
    public ISchedulingRule getSchedulingRule() {
        final IResource resource = getResource();
        if (resource == null) {
            return new NoResourceSchedulingRule(getResource()
                    .getProjectRelativePath());
        }
        return resource;
    }

    /**
     * @see IParent
     */
    public boolean hasChildren() {
        // if I am not open, return true to avoid opening (case of an Erlang
        // project, a compilation unit or a class file).
        // also see https://bugs.eclipse.org/bugs/show_bug.cgi?id=52474
        final Object elementInfo = ErlangCore.getModelManager().getInfo(this);
        if (elementInfo instanceof ErlElement) {
            return !internalGetChildren().isEmpty();
        }
        return true;
    }

    public boolean hasChildrenOfKind(final Kind kind) {
        for (final IErlElement child : internalGetChildren()) {
            if (child.getKind() == kind) {
                return true;
            }
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
        if (internalGetChildren().size() > 0) {
            buffer.append("{");
            int i = 0;
            for (final IErlElement element : internalGetChildren()) {
                ((ErlElement) element).toString(tab + 1, buffer);
                buffer.append(","); //$NON-NLS-1$
                if (++i > 3) {
                    buffer.append("...");
                    break;
                }
            }
            buffer.deleteCharAt(buffer.length() - 1);
            buffer.append("}");
        }
    }

    /**
     * Debugging purposes
     */
    public Object toStringInfo(final int tab, final StringBuilder buffer) {
        final Object info = ErlangCore.getModelManager().getInfo(this);
        this.toStringInfo(tab, buffer, info);
        return info;
    }

    /**
     * Debugging purposes
     */
    protected void toStringInfo(final int tab, final StringBuilder buffer,
            final Object info) {
        buffer.append(tabString(tab));
        toStringName(buffer);
        if (info == null) {
            buffer.append(" (not open)"); //$NON-NLS-1$
        }
    }

    /**
     * Debugging purposes
     */
    protected void toStringName(final StringBuilder buffer) {
        buffer.append(getName());
        if (fOccurrenceCount > 1) {
            buffer.append("#"); //$NON-NLS-1$
            buffer.append(fOccurrenceCount);
        }
    }

    // /**
    // * Collection of handles of immediate children of this object. This is an
    // * empty array if this element has no children.
    // */
    // private final List<IErlElement> fChildren = new ArrayList<IErlElement>();

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
        return Collections.unmodifiableList(Lists
                .newArrayList(internalGetChildren()));
    }

    protected List<IErlElement> internalGetChildren() {
        return fChildren;
    }

    public int getChildCount() {
        return internalGetChildren().size();
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
        for (final IErlElement element : internalGetChildren()) {
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
        return internalGetChildren().contains(child);
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

    public void removeChildren() {
        clearCaches();
        fChildren.clear();
    }

    public void addChild(final IErlElement child) {
        clearCaches();
        fChildren.add(child);
    }

    public void setChildren(final Collection<? extends IErlElement> children) {
        clearCaches();
        fChildren.clear();
        fChildren.addAll(children);
    }

    public void setChildren(final IErlElement[] children) {
        setChildren(Arrays.asList(children));
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
        for (final IErlElement child : parent.internalGetChildren()) {
            if (child.getName().equals(name)) {
                return child;
            }
        }
        return null;
    }

    private static IErlElement getChildWithResource(final ErlElement parent,
            final IResource rsrc) {
        for (final IErlElement child : parent.internalGetChildren()) {
            if (rsrc.equals(child.getResource())) {
                return child;
            }
        }
        return null;
    }

    public final void accept(final IErlElementVisitor visitor,
            final EnumSet<AcceptFlags> flags, final IErlElement.Kind leafKind)
            throws ErlModelException {
        getModel().accept(this, visitor, flags, leafKind);
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

    /*
     * (non-Javadoc)
     * 
     * @see org.erlide.core.model.erlang.IErlElement#getFilePath()
     */
    public String getFilePath() {
        return null;
    }

    public String getModuleName() {
        final IErlModule module = getModule();
        if (module != null) {
            return module.getName();
        }
        final String path = getFilePath();
        if (path != null) {
            return new Path(path).lastSegment();
        }
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
