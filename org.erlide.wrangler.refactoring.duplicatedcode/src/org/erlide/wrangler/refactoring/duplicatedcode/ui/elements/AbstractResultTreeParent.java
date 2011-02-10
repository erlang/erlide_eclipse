/**
 * 
 */
package org.erlide.wrangler.refactoring.duplicatedcode.ui.elements;

import java.util.ArrayList;

/**
 * A tree element which can contain other elements
 * 
 * @author Gyorgy Orosz
 * 
 */
public abstract class AbstractResultTreeParent extends AbstractResultTreeObject {
    protected ArrayList<AbstractResultTreeObject> children = null;

    /*
     * public ResultTreeParent(String name) { super(name); children = new
     * ArrayList(); }
     */

    /**
     * Adds a child element
     * 
     * @param child
     *            child element
     */
    public void addChild(final AbstractResultTreeObject child) {
        if (children == null) {
            children = new ArrayList<AbstractResultTreeObject>();
        }
        children.add(child);
        child.setParent(this);
    }

    /**
     * Removes a child element
     * 
     * @param child
     *            child element
     */
    public void removeChild(final AbstractResultTreeObject child) {
        if (children != null) {
            children.remove(child);
            child.setParent(null);
        }
    }

    /**
     * Return the children of the the current element
     * 
     * @return children elmements
     */
    public AbstractResultTreeObject[] getChildren() {
        if (children != null) {
            return children.toArray(new AbstractResultTreeObject[children
                    .size()]);
        } else {
            return new AbstractResultTreeObject[0];
        }
    }

    /**
     * Return true if the element has children
     * 
     * @return true, if has any child element
     */
    public boolean hasChildren() {
        return children != null && children.size() > 0;
    }

}
