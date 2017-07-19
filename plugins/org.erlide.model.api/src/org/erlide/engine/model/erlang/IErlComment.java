/**
 *
 */
package org.erlide.engine.model.erlang;

/**
 * @author jakob
 *
 * @noimplement This interface is not intended to be implemented by clients.
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
