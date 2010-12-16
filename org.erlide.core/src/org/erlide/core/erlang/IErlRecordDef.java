/**
 * 
 */
package org.erlide.core.erlang;

/**
 * @author jakob
 * 
 */
public interface IErlRecordDef extends IErlPreprocessorDef {

    IErlRecordField getFieldNamed(String name);

}
