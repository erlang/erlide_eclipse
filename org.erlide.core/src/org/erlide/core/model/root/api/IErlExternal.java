/**
 * 
 */
package org.erlide.core.model.root.api;


/**
 * @author jakob
 * 
 */
public interface IErlExternal extends IErlElement, IParent, IOpenable {

    String getExternalName();

    boolean isOTP();

    boolean hasIncludes();
}
