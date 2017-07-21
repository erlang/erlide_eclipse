package org.erlide.engine.model.erlang;

/**
 *
 * @author Vlad
 *
 * @noimplement This interface is not intended to be implemented by clients.
 */
public interface IErlRecordField extends IErlMember {

    String getFieldName();

    String getExtra();

}
