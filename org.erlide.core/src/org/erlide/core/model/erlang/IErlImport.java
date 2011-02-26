/**
 * 
 */
package org.erlide.core.model.erlang;

import org.erlide.jinterface.util.IConvertible;

/**
 * @author jakob
 * 
 */
public interface IErlImport extends IErlImportExport, IErlMember, IParent,
        IConvertible {
    public String getImportModule();
}
