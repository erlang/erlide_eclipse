/**
 * 
 */
package org.erlide.engine.model.root;

import org.erlide.engine.model.IOpenable;
import org.erlide.engine.model.IParent;

/**
 * @author jakob
 * 
 */
public interface IErlExternal extends IErlElement, IParent, IOpenable {

    boolean isOTP();

    boolean hasIncludes();
}
