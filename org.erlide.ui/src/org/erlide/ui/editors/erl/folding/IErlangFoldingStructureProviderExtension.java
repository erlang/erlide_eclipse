/**
 * 
 */
package org.erlide.ui.editors.erl.folding;

import org.erlide.core.model.root.IErlElement;

/**
 * @author jakob
 * 
 */
public interface IErlangFoldingStructureProviderExtension {

    /**
     * Collapses all members except for top level types.
     */
    void collapseFunctions();

    /**
     * Expand all
     */
    void expandAll();

    /**
     * Collapses all comments.
     */
    void collapseComments();

    /**
     * Collapses the given elements.
     * 
     * @param elements
     *            the java elements to collapse (the array and its elements will
     *            not be modified)
     */
    void collapseElements(IErlElement[] elements);

    /**
     * Expands the given elements.
     * 
     * @param elements
     *            the java elements to expand (the array and its elements will
     *            not be modified)
     */
    void expandElements(IErlElement[] elements);
}
