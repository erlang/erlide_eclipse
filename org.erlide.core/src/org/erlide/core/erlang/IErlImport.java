/**
 * 
 */
package org.erlide.core.erlang;

import java.util.List;

import org.erlide.core.util.ErlangFunction;
import org.erlide.jinterface.rpc.IConvertible;

/**
 * @author jakob
 * 
 */
public interface IErlImport extends IErlMember, IParent, IConvertible {

	public boolean hasImported(ErlangFunction f);

	public String getImportModule();

	public List<ErlangFunction> getFunctions();
}
