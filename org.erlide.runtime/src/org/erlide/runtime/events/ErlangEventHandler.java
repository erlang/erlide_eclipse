package org.erlide.runtime.events;

import org.erlide.util.IDisposable;

public abstract class ErlangEventHandler implements IDisposable {
    private final String backendName;
    private final String topic;

    public ErlangEventHandler(final String topic, final String backendName) {
        this.topic = topic;
        this.backendName = backendName;
    }

    @Override
    public void dispose() {
    }

    public String getBackendName() {
        return backendName;
    }

    public String getTopic() {
        return topic;
    }
}
