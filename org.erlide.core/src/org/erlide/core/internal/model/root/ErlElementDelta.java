package org.erlide.core.internal.model.root;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IResourceDelta;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlElementDelta;
import org.erlide.core.model.root.IParent;

import com.google.common.collect.Lists;

public class ErlElementDelta implements IErlElementDelta {

    private int fKind;

    private int fFlags;

    private final IErlElement fElement;

    private final ArrayList<ErlElementDelta> fChildren;

    private ArrayList<IResourceDelta> fResourceDeltas;

    /**
     * @see #getMovedFromElement()
     */
    protected IErlElement fMovedFromElement = null;

    /**
     * @see #getMovedToElement()
     */
    protected IErlElement fMovedToElement = null;

    /**
     * @param kind
     * @param flags
     * @param element
     */
    public ErlElementDelta(final int kind, final int flags,
            final IErlElement element) {
        this(kind, flags, element, new ArrayList<ErlElementDelta>(0),
                new ArrayList<IResourceDelta>(0));
    }

    /**
     * @param kind
     * @param flags
     * @param element
     * @param children
     */
    public ErlElementDelta(final int kind, final int flags,
            final IErlElement element, final ArrayList<ErlElementDelta> children) {
        this(kind, flags, element, children, new ArrayList<IResourceDelta>(0));
    }

    /**
     * @param kind
     * @param flags
     * @param element
     * @param children
     * @param resourceDeltas
     */
    public ErlElementDelta(final int kind, final int flags,
            final IErlElement element,
            final ArrayList<ErlElementDelta> children,
            final ArrayList<IResourceDelta> resourceDeltas) {
        super();
        fKind = kind;
        fFlags = flags;
        fElement = element;
        fChildren = children;
        fResourceDeltas = resourceDeltas;
    }

    @Override
    public IErlElementDelta[] getChildren(final int kind) {
        final ArrayList<IErlElementDelta> children = new ArrayList<IErlElementDelta>(
                0);
        for (int i = 0; i < fChildren.size(); ++i) {
            final IErlElementDelta c = fChildren.get(i);
            if (c.getKind() == kind || kind == ALL) {
                children.add(c);
            }
        }
        return children.toArray(new IErlElementDelta[children.size()]);
    }

    @Override
    public IErlElement getElement() {
        return fElement;
    }

    @Override
    public int getFlags() {
        return fFlags;
    }

    @Override
    public int getKind() {
        return fKind;
    }

    @Override
    public IResourceDelta[] getResourceDeltas() {
        return fResourceDeltas.toArray(new IResourceDelta[fResourceDeltas
                .size()]);
    }

    /**
     * Creates the delta tree for the given element and delta, and then inserts
     * the tree as an affected child of this node.
     */
    public void insertDeltaTree(final IErlElement element,
            final ErlElementDelta delta) {
        final ErlElementDelta childDelta = createDeltaTree(element, delta);
        if (!equalsAndSameParent(element, getElement())) {
            addAffectedChild(childDelta);
        }
    }

    /**
     * Creates the nested delta deltas based on the affected element its delta,
     * and the root of this delta tree. Returns the root of the created delta
     * tree.
     */
    protected ErlElementDelta createDeltaTree(final IErlElement element,
            final ErlElementDelta delta) {
        ErlElementDelta childDelta = delta;
        final List<IParent> ancestors = getAncestors(element);
        if (ancestors == null) {
            if (equalsAndSameParent(delta.getElement(), getElement())) {
                // handle case of two jars that can be equals but not in the
                // same project
                // the element being changed is the root element
                fKind = delta.fKind;
                fFlags = delta.fFlags;
                fMovedToElement = delta.fMovedToElement;
                fMovedFromElement = delta.fMovedFromElement;
            } else {
                // the given delta is not the root or a child - illegal
                // Assert.isTrue(false);
            }
        } else {
            for (final IParent ancestor : ancestors) {
                final IErlElement element2 = (IErlElement) ancestor;
                final ErlElementDelta ancestorDelta = new ErlElementDelta(0, 0,
                        element2);
                ancestorDelta.addAffectedChild(childDelta);
                childDelta = ancestorDelta;
            }
        }
        return childDelta;
    }

    @Override
    public IErlElement getMovedFromElement() {
        return fMovedFromElement;
    }

    @Override
    public IErlElement getMovedToElement() {
        return fMovedToElement;
    }

    /**
     * Adds the child delta to the collection of affected children. If the child
     * is already in the collection, walk down the hierarchy.
     * 
     * JC: this sucks a little bit, too much code, as always... (from C model,
     * not my fault)
     */
    protected void addAffectedChild(final ErlElementDelta child) {
        switch (fKind) {
        case ADDED:
        case REMOVED:
            // no need to add a child if this parent is added or removed
            return;
        case CHANGED:
            fFlags |= F_CHILDREN;
            break;
        default:
            fKind = CHANGED;
            fFlags |= F_CHILDREN;
            break;
        }

        // Check if we already have the delta.
        IErlElementDelta existingChild = null;
        int existingChildIndex = -1;
        for (int i = 0; i < fChildren.size(); i++) {
            // handle case of two jars that can be equals but not in the same
            // project
            if (equalsAndSameParent(fChildren.get(i).getElement(),
                    child.getElement())) {
                existingChild = fChildren.get(i);
                existingChildIndex = i;
                break;
            }
        }

        if (existingChild == null) { // new affected child
            fChildren.add(child);
        } else {
            switch (existingChild.getKind()) {
            case ADDED:
                switch (child.getKind()) {
                // child was added then added -> it is added
                case ADDED:
                    // child was added then changed -> it is added
                case CHANGED:
                    return;

                    // child was added then removed -> noop
                case REMOVED:
                    fChildren.remove(existingChildIndex);
                    return;
                }
                break;
            case REMOVED:
                switch (child.getKind()) {
                // child was removed then added -> it is changed
                case ADDED:
                    child.fKind = CHANGED;
                    fChildren.set(existingChildIndex, child);
                    return;

                    // child was removed then changed -> it is removed
                case CHANGED:
                    // child was removed then removed -> it is removed
                case REMOVED:
                    return;
                }
                break;
            case CHANGED:
                switch (child.getKind()) {
                // child was changed then added -> it is added
                case ADDED:
                    // child was changed then removed -> it is removed
                case REMOVED:
                    fChildren.set(existingChildIndex, child);
                    return;

                    // child was changed then changed -> it is changed
                case CHANGED:
                    final IErlElementDelta[] children = child.getChildren(ALL);
                    for (final IErlElementDelta element : children) {
                        final ErlElementDelta childsChild = (ErlElementDelta) element;
                        ((ErlElementDelta) existingChild)
                                .addAffectedChild(childsChild);
                    }
                    // add the non-erlang resource deltas if needed
                    // note that the child delta always takes
                    // precedence over this existing child delta
                    // as non-erlang resource deltas are always
                    // created last (by the DeltaProcessor)
                    // FIXME JC: which means??
                    ((ErlElementDelta) existingChild).fResourceDeltas = child.fResourceDeltas;
                    return;
                }
                break;
            default:
                // unknown -> existing child becomes the child with the existing
                // child's
                // flags
                final int flags = existingChild.getFlags();
                fChildren.set(existingChildIndex, child);
                child.fFlags |= flags;
            }
        }
    }

    private List<IParent> getAncestors(IErlElement element) {
        IParent parent = element.getParent();
        if (parent == null) {
            return null;
        }
        final ArrayList<IParent> parents = Lists.newArrayList();
        while (!parent.equals(fElement)) {
            parents.add(parent);
            if (parent instanceof IErlElement) {
                element = (IErlElement) parent;
                parent = element.getParent();
            } else {
                break;
            }
            if (parent == null) {
                break;
            }
        }
        parents.trimToSize();
        return parents;
    }

    /**
     * Returns whether the two elements are equals and have the same parent.
     */
    protected boolean equalsAndSameParent(final IErlElement e1,
            final IErlElement e2) {
        IErlElement parent1;
        return e1.equals(e2)
                && (parent1 = (IErlElement) e1.getParent()) != null
                && parent1.equals(e2.getParent());
    }

    /**
     * Adds the child delta to the collection of affected children. If the child
     * is already in the collection, walk down the hierarchy.
     */
    public void addResourceDelta(final IResourceDelta child) {
        switch (fKind) {
        case ADDED:
        case REMOVED:
            // no need to add a child if this parent is added or removed
            return;
        case CHANGED:
            fFlags |= F_CONTENT;
            break;
        default:
            fKind = CHANGED;
            fFlags |= F_CONTENT;
        }
        fResourceDeltas.add(child);
    }

    /**
     * Creates the nested deltas resulting from a change operation. Convenience
     * method for creating change deltas. The constructor should be used to
     * create the root delta and then a change operation should call this
     * method.
     */
    public void changed(final IErlElement element, final int flag) {
        final ErlElementDelta changedDelta = new ErlElementDelta(0, 0, element);
        changedDelta.fKind = CHANGED;
        changedDelta.fFlags |= flag;
        insertDeltaTree(element, changedDelta);
    }

    @Override
    public IErlElementDelta findElement(final IErlElement element) {
        if (fElement.equals(element)) {
            return this;
        }
        for (int i = 0; i < fChildren.size(); ++i) {
            final IErlElementDelta d = fChildren.get(i).findElement(element);
            if (d != null) {
                return d;
            }
        }
        return null;
    }

}
