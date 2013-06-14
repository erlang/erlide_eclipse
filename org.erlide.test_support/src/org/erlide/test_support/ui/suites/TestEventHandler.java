package org.erlide.test_support.ui.suites;

import org.erlide.runtime.events.ErlEvent;
import org.erlide.runtime.events.ErlangEventHandler;

import com.google.common.eventbus.Subscribe;

public class TestEventHandler extends ErlangEventHandler {

    private final TestResultsView view;

    public TestEventHandler(final String backendName, final TestResultsView view) {
        super("bterl", backendName);
        this.view = view;
    }

    @Subscribe
    public void handleEvent(final ErlEvent event) {
        if (!event.getTopic().equals(getTopic())) {
            return;
        }
        if (view != null) {
            view.notifyEvent(event.getEvent());
        }
    }
}
