package org.erlide.backend.events;

import java.util.Dictionary;
import java.util.Hashtable;

import org.erlide.backend.IBackend;
import org.erlide.core.ErlangPlugin;
import org.erlide.utils.IDisposable;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceRegistration;
import org.osgi.service.event.EventConstants;
import org.osgi.service.event.EventHandler;

public abstract class ErlangEventHandler implements EventHandler, IDisposable {
    private final IBackend backend;
    private final String topic;
    private ServiceRegistration registration;

    public ErlangEventHandler(final String topic, final IBackend backend) {
        this.topic = topic;
        this.backend = backend;
    }

    public void register() {
        final String fullTopic = ErlangEventPublisher.getFullTopic(topic,
                backend);
        // ErlLogger.info("Register event handler for " + topic + ": " + this);
        final BundleContext context = ErlangPlugin.getDefault().getBundle()
                .getBundleContext();
        if (registration == null) {
            final Dictionary<String, String> properties = new Hashtable<String, String>();
            properties.put(EventConstants.EVENT_TOPIC, fullTopic);
            registration = context.registerService(
                    EventHandler.class.getName(), this, properties);
        }
    }

    public IBackend getBackend() {
        return backend;
    }

    @Override
    public void dispose() {
        registration.unregister();
    }
}
