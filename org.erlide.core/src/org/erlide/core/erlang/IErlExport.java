/**
 * 
 */
package org.erlide.core.erlang;

import org.erlide.core.erlang.util.ErlangFunction;

/**
 * @author jakob
 * 
 */
public interface IErlExport extends IErlImportExport, IErlMember, IParent {
    public boolean hasFunction(final ErlangFunction f);
}
