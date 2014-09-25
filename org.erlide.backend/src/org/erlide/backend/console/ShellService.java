package org.erlide.backend.console;

import org.erlide.runtime.api.IOtpNodeProxy;

import com.ericsson.otp.erlang.OtpErlangPid;

public interface ShellService {

    OtpErlangPid start(IOtpNodeProxy runtime);

}
