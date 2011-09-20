/**
 * 
 */
package org.erlide.core.model.erlang;

import org.erlide.core.model.root.IParent;

/**
 * @author jakob
 * 
 */
public interface IErlImport extends IErlImportExport, IErlMember, IParent {
    public String getImportModule();
}
