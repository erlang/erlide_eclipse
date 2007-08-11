package org.erlide.runtime.backend;

import java.util.List;

import com.ericsson.otp.erlang.OtpErlangObject;

public interface IErlEventListener {
	void handleEvent(List<OtpErlangObject> msgs);
}
