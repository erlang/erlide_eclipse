/**
 * 
 */
package org.erlide.core.erlang;

import org.erlide.jinterface.rpc.IConvertible;

/**
 * @author jakob
 * 
 */
public interface IErlImport extends IErlImportExport, IErlMember, IParent,
		IConvertible {
	public String getImportModule();
}
