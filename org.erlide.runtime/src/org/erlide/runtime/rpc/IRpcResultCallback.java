package org.erlide.runtime.rpc;

import com.ericsson.otp.erlang.OtpErlangObject;

public interface IRpcResultCallback {

    void start(final OtpErlangObject msg);

    void stop(final OtpErlangObject msg);

    void progress(final OtpErlangObject msg);

}
