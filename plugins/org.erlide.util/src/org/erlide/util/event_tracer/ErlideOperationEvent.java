package org.erlide.util.event_tracer;

@SuppressWarnings("all")
public abstract class ErlideOperationEvent extends ErlideEvent {
    protected final String operation;

    protected final String id;

    public ErlideOperationEvent(final String myOperation, final String myId) {
        super(System.currentTimeMillis());
        operation = myOperation;
        id = myId;
    }
}
