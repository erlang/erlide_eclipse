/**
 * 
 */
package org.erlide.core.model.erlang;


/**
 * @author jakob
 * 
 */
public interface IErlImport extends IErlImportExport, IErlMember, IParent {
    public String getImportModule();
}
