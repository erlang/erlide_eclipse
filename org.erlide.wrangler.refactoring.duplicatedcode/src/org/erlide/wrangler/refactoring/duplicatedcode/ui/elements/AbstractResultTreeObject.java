/**
 * 
 */
package org.erlide.wrangler.refactoring.duplicatedcode.ui.elements;

import org.eclipse.core.runtime.IAdaptable;

/**
 * Abstract element of the duplicates view Treeview
 * 
 * @author Gyorgy Orosz
 * 
 */
public abstract class AbstractResultTreeObject implements IAdaptable {
    // private String name;
    private AbstractResultTreeParent parent;

    /*
     * public abstract ResultTreeObject(String name); { this.name = name; }
     */

    /**
     * Returns the name of the element.
     * 
     * @return name of the element
     */
    public abstract String getName(); /*
                                       * { return name; }
                                       */

    /**
     * Sets the parent of the current element.
     * 
     * @param parent
     *            parent of the current item
     * 
     */
    public void setParent(final AbstractResultTreeParent parent) {
        this.parent = parent;
    }

    /**
     * Get the parent item of the current
     * 
     * @return parent item
     */
    public AbstractResultTreeParent getParent() {
        return parent;
    }

    @Override
    public String toString() {
        return getName();
    }

    @Override
    public Object getAdapter(@SuppressWarnings("rawtypes") final Class adapter) {
        return null;
    }

    String suggestedCode = "";

    /**
     * Get the codepart suggested by Wrangler
     * 
     * @return suggested codepart stringss
     */
    public String getSuggestedCode() {
        return suggestedCode;
    }

    /**
     * Set the suggested code part fromWrangler
     * 
     * @param str
     *            suggested code part string
     */
    public void setSuggestedCode(final String str) {
        suggestedCode = str;
    }

}
