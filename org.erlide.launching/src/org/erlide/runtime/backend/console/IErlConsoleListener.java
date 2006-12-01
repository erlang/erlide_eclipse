package org.erlide.runtime.backend.console;

import java.util.List;

import com.ericsson.otp.erlang.OtpErlangObject;

public interface IErlConsoleListener {
	void handleEvent(List<OtpErlangObject> msgs);
}
