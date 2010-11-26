/**
 * 
 */
package org.erlide.core.erlang;

/**
 * @author jakob
 * 
 */
public interface IErlComment extends IErlMember {

    String getHoverHelp();

    boolean isHeader();

    boolean isEdoc();

    /**
     * The number of %s preceding the comment. Useful for correct indentation
     * 
     * @return the number of %s preceding the comment
     */
    // int getLevel();
}
