package org.erlide.engine.model.erlang;

import org.erlide.engine.model.root.IErlElement;

/**
 * @author jakob
 * 
 */
public interface IErlangFirstThat {

    /**
     * A boolean function returning true for the element that we're looking for
     * 
     * @param e
     * @return
     */
    boolean firstThat(IErlElement e);

}
