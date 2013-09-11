package org.erlide.backend.console;

import org.erlide.runtime.api.IErlRuntime;

import com.ericsson.otp.erlang.OtpErlangPid;

public interface ShellService {

    OtpErlangPid start(IErlRuntime runtime);

}
