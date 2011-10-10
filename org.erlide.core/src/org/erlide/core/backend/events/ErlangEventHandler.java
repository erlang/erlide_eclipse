package org.erlide.core.backend.events;

import java.util.Dictionary;
import java.util.Hashtable;

import org.erlide.core.ErlangPlugin;
import org.erlide.core.backend.IBackend;
import org.osgi.framework.BundleContext;
import org.osgi.service.event.EventConstants;
import org.osgi.service.event.EventHandler;

public abstract class ErlangEventHandler implements EventHandler {
    private final IBackend backend;
    private final String topic;

    public ErlangEventHandler(final String topic, final IBackend backend) {
        this.topic = topic;
        this.backend = backend;
    }

    private String getErlangTopic() {
        return topic;
    }

    public void register() {
        final String fullTopic = ErlangEventPublisher.getFullTopic(topic,
                backend);
        final Dictionary<String, String> properties = new Hashtable<String, String>();
        properties.put(EventConstants.EVENT_TOPIC, fullTopic);
        final BundleContext context = ErlangPlugin.getDefault().getBundle()
                .getBundleContext();
        context.registerService(EventHandler.class.getName(), this, properties);
    }

    public IBackend getBackend() {
        return backend;
    }
}
