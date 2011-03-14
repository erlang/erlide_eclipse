/**
 * 
 */
package org.erlide.core.model.erlang;

import org.erlide.jinterface.IConvertible;

/**
 * @author jakob
 * 
 */
public interface IErlImport extends IErlImportExport, IErlMember, IParent,
        IConvertible {
    public String getImportModule();
}
