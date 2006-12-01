/**
 * 
 */
package org.erlide.core.erlang;

import org.erlide.core.util.ErlangFunction;

/**
 * @author jakob
 * 
 */
public interface IErlImport extends IErlMember, IParent {

	public boolean hasImported(ErlangFunction f);

	public String getImportModule();

	public ErlangFunction[] getFunctions();
}
