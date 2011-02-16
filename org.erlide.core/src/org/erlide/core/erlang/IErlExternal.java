/**
 * 
 */
package org.erlide.core.erlang;

import org.erlide.jinterface.backend.Backend;

/**
 * @author jakob
 * 
 */
public interface IErlExternal extends IErlElement, IParent, IOpenable {

    String getExternalName();

    Backend getBackend();

    boolean isOTP();
}
