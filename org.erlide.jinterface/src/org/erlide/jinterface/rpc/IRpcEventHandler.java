package org.erlide.jinterface.rpc;

import com.ericsson.otp.erlang.OtpErlangObject;

public interface IRpcEventHandler {

    /**
     * Handle events from erlang. Return true to terminate. Should be stateless.
     * 
     * @param msg
     *            The term sent from erlang, can't be null
     * 
     */
    boolean handleEvent(OtpErlangObject msg);

}
