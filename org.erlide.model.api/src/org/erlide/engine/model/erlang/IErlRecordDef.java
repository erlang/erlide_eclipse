/**
 *
 */
package org.erlide.engine.model.erlang;

/**
 * @author jakob
 *
 * @noimplement This interface is not intended to be implemented by clients.
 *
 */
public interface IErlRecordDef extends IErlPreprocessorDef {

    IErlRecordField getFieldNamed(String name);

}
