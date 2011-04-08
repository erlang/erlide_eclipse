package org.erlide.jinterface;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * Implementors should also implement
 * <code>public static SomeObject fromErlangObject(OtpErlangObject obj);</code>
 * if they support converting from Erlang objects.
 * 
 * The static method can't be added to the interface and is found by reflection.
 */
public interface IConvertible {

    public OtpErlangObject toErlangObject();

}
