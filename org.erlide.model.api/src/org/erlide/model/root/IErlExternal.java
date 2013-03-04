/**
 * 
 */
package org.erlide.model.root;

import org.erlide.model.IOpenable;
import org.erlide.model.IParent;

/**
 * @author jakob
 * 
 */
public interface IErlExternal extends IErlElement, IParent, IOpenable {

    boolean isOTP();

    boolean hasIncludes();
}
