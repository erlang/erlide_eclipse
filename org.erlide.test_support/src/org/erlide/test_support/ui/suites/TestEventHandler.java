package org.erlide.test_support.ui.suites;

import org.erlide.jinterface.backend.events.ErlangEvent;
import org.erlide.jinterface.backend.events.EventHandler;

public class TestEventHandler extends EventHandler {

    private final ResultsView view;

    public TestEventHandler(final ResultsView view) {
        this.view = view;
    }

    @Override
    protected void doHandleEvent(ErlangEvent event) throws Exception {
        if (!event.hasTopic("bterl")) {
            return;
        }
        if (view != null) {
            view.notifyEvent(event.data);
        }
    }

}
