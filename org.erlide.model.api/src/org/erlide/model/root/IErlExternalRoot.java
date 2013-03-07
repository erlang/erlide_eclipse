/**
 * 
 */
package org.erlide.model.root;

import java.util.List;

import org.erlide.model.IOpenable;
import org.erlide.model.IParent;

/**
 * @author jakob
 * 
 */
public interface IErlExternalRoot extends IErlExternal, IErlElement, IParent,
		IOpenable {

	List<IErlElement> internalGetChildren();

}
