package org.erlide.runtime.events;

import org.erlide.util.IDisposable;

public abstract class ErlangEventHandler implements IDisposable {
    private final String topic;

    public ErlangEventHandler(final String topic) {
        this.topic = topic;
    }

    @Override
    public void dispose() {
    }

    public String getTopic() {
        return topic;
    }
}
