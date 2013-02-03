/**
 * 
 */
package org.erlide.core.model.root;

import org.erlide.core.model.IOpenable;
import org.erlide.core.model.IParent;

/**
 * @author jakob
 * 
 */
public interface IErlExternal extends IErlElement, IParent, IOpenable {

    boolean isOTP();

    boolean hasIncludes();
}
