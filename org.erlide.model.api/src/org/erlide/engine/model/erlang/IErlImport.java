/**
 * 
 */
package org.erlide.engine.model.erlang;

import org.erlide.engine.IParent;

/**
 * @author jakob
 * 
 */
public interface IErlImport extends IErlImportExport, IErlMember, IParent {
    public String getImportModule();
}
