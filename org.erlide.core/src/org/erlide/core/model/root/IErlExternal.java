/**
 * 
 */
package org.erlide.core.model.root;

/**
 * @author jakob
 * 
 */
public interface IErlExternal extends IErlElement, IParent, IOpenable {

    boolean isOTP();

    boolean hasIncludes();
}
