/**
 * 
 */
package org.erlide.core.model.erlang;

import org.erlide.core.backend.RpcCallSite;

/**
 * @author jakob
 * 
 */
public interface IErlExternal extends IErlElement, IParent, IOpenable {

    String getExternalName();

    RpcCallSite getBackend();

    boolean isOTP();

    boolean hasIncludes();
}
