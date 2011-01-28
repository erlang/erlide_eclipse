package org.erlide.test_support.ui.suites;

import org.erlide.jinterface.backend.events.EventHandler;

import com.ericsson.otp.erlang.OtpErlangObject;

public class TestEventHandler extends EventHandler {

    private final ResultsView view;

    public TestEventHandler(final ResultsView view) {
        this.view = view;
    }

    @Override
    protected void doHandleMsg(final OtpErlangObject msg) throws Exception {
        final OtpErlangObject event = getStandardEvent(msg, "bterl");
        if (event == null) {
            return;
        }
        if (view != null) {
            view.notifyEvent(msg);
        }
    }

}
