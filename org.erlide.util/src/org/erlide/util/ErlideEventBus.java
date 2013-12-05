package org.erlide.util;

import com.google.common.eventbus.DeadEvent;
import com.google.common.eventbus.EventBus;
import com.google.common.eventbus.Subscribe;

public class ErlideEventBus {

    private static EventBus bus;

    private static EventBus bus() {
        if (bus == null) {
            bus = new EventBus("erlide");
            bus.register(new ErlideEventBus());
        }
        return bus;
    }

    public static void post(final Object event) {
        bus().post(event);
    }

    public static void register(final Object listener) {
        bus().register(listener);
    }

    public static void unregister(final Object listener) {
        bus().unregister(listener);
    }

    @Subscribe
    public void deadEvent(final DeadEvent event) {
        ErlLogger.warn("no handler for " + event.getEvent().getClass().getName());
    }

}
