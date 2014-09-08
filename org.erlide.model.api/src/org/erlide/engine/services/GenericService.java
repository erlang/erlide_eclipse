package org.erlide.engine.services;

import com.ericsson.otp.erlang.OtpErlangObject;

public interface GenericService extends ErlangService {

    OtpErlangObject call(String fErlModule, String fErlFunction, int offset, int length,
            String aText);

}
