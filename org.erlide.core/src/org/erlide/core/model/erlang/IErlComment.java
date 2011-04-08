/**
 * 
 */
package org.erlide.core.model.erlang;

/**
 * @author jakob
 * 
 */
public interface IErlComment extends IErlMember {

    // String getHoverHelp();

    boolean isHeader();

    /**
     * The number of %s preceding the comment. Useful for correct indentation
     * 
     * @return the number of %s preceding the comment
     */
    // int getLevel();
}
