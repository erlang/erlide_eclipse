/**
 * 
 */
package org.erlide.model.erlang;

import org.erlide.model.IParent;

/**
 * @author jakob
 * 
 */
public interface IErlImport extends IErlImportExport, IErlMember, IParent {
    public String getImportModule();
}
