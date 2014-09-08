/**
 *
 */
package org.erlide.engine.model.root;

import org.erlide.engine.model.IOpenable;
import org.erlide.engine.model.IParent;

/**
 * @author jakob
 *
 * @noimplement This interface is not intended to be implemented by clients.
 *
 */
public interface IErlExternal extends IErlElement, IParent, IOpenable {

    boolean isOTP();

    boolean hasIncludes();
}
