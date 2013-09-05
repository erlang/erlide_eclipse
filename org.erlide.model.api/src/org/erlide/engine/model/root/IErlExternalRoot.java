/**
 *
 */
package org.erlide.engine.model.root;

import java.util.List;

/**
 * @author jakob
 * 
 */
public interface IErlExternalRoot extends IErlExternal {

    List<IErlElement> internalGetChildren();

}
