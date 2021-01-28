package org.erlide.util.event_tracer;

@SuppressWarnings("all")
public class NullEventHandler extends ErlideEventTracerHandler {
    public NullEventHandler() {
        super(null);
    }

    @Override
    public void handle(final ErlideEvent event) {
    }

    @Override
    public void dispose() {
    }
}
