/**
 * 
 */
package org.erlide.core.model.erlang;


/**
 * @author jakob
 * 
 */
public interface IErlExternal extends IErlElement, IParent, IOpenable {

    String getExternalName();

    boolean isOTP();

    boolean hasIncludes();
}
