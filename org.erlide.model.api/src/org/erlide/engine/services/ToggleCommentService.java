package org.erlide.engine.services;

import com.ericsson.otp.erlang.OtpErlangObject;

// TODO implement this in Java, there is no need to do it in Erlang
public interface ToggleCommentService extends ErlangService {

    OtpErlangObject call(String fErlModule, String fErlFunction, int offset, int length,
            String aText);

}
