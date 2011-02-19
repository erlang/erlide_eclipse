/**
 * 
 */
package org.erlide.core.erlang;

import org.erlide.backend.rpc.RpcCallSite;

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
