/**
 *
 */
package org.erlide.engine.model.root;

import java.util.List;

import org.erlide.engine.model.IErlElement;

/**
 * @author jakob
 *
 * @noimplement This interface is not intended to be implemented by clients.
 *
 */
public interface IErlExternalRoot extends IErlExternal {

    List<IErlElement> internalGetChildren();

}
