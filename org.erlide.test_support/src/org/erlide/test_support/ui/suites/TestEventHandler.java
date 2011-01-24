package org.erlide.test_support.ui.suites;

import org.erlide.jinterface.backend.events.EventHandler;

import com.ericsson.otp.erlang.OtpErlangObject;

public class TestEventHandler extends EventHandler {

    @Override
    protected void doHandleMsg(final OtpErlangObject msg) throws Exception {
        final OtpErlangObject event = getStandardEvent(msg, "bterl");
        if (event == null) {
            return;
        }
        System.out.println("!!!!!!!!!!" + event);
    }

}
