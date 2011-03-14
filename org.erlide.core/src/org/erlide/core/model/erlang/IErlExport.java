/**
 * 
 */
package org.erlide.core.model.erlang;

import org.erlide.core.model.erlang.util.ErlangFunction;

/**
 * @author jakob
 * 
 */
public interface IErlExport extends IErlImportExport, IErlMember, IParent {
    public boolean hasFunction(final ErlangFunction f);
}
