package org.erlide.engine.model.root;

import org.erlide.engine.model.ErlModelException;

/**
 * Interface for visitors in the erlang model tree
 *
 * @author jakob
 *
 */
public interface IErlElementVisitor {
    /**
     * Visits the given element.
     *
     * @param element
     *            the element to visit
     * @return <code>true</code> if the children should be visited
     *         <code>false</code> if they should be skipped
     * @throws ErlModelException
     */
    boolean visit(IErlElement element) throws ErlModelException;

}
