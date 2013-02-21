package org.erlide.test_support.ui.suites;

import org.erlide.backend.events.ErlangEventHandler;
import org.osgi.service.event.Event;

import com.ericsson.otp.erlang.OtpErlangObject;

public class TestEventHandler extends ErlangEventHandler {

    private final TestResultsView view;

    public TestEventHandler(final String backendName, final TestResultsView view) {
        super("bterl", backendName);
        this.view = view;
    }

    @Override
    public void handleEvent(final Event event) {
        if (view != null) {
            view.notifyEvent((OtpErlangObject) event.getProperty("DATA"));
        }
    }
}
