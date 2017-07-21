/**
 *
 */
package org.erlide.engine.model.erlang;

import org.erlide.engine.model.IParent;

/**
 * @author jakob
 *
 * @noimplement This interface is not intended to be implemented by clients.
 *
 */
public interface IErlImport extends IErlImportExport, IErlMember, IParent {
    public String getImportModule();
}
