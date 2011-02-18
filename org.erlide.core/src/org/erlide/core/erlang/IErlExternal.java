/**
 * 
 */
package org.erlide.core.erlang;

import org.erlide.backend.ErlCallable;

/**
 * @author jakob
 * 
 */
public interface IErlExternal extends IErlElement, IParent, IOpenable {

    String getExternalName();

    ErlCallable getBackend();

    boolean isOTP();

    boolean hasHeaders();
}
