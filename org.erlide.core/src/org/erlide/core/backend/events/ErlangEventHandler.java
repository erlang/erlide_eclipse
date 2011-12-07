package org.erlide.core.backend.events;

import java.util.Dictionary;
import java.util.Hashtable;

import org.erlide.core.ErlangPlugin;
import org.erlide.core.backend.IBackend;
import org.erlide.jinterface.ErlLogger;
import org.osgi.framework.BundleContext;
import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceReference;
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
        ErlLogger.info("Register event handler for " + topic + ": " + this);
        final BundleContext context = ErlangPlugin.getDefault().getBundle()
                .getBundleContext();
        try {
            final ServiceReference[] refs = context.getServiceReferences(
                    EventHandler.class.getName(), "("
                            + EventConstants.EVENT_TOPIC + "=" + fullTopic
                            + ")");
            if (refs == null || !contains(refs, this)) {
                final Dictionary<String, String> properties = new Hashtable<String, String>();
                properties.put(EventConstants.EVENT_TOPIC, fullTopic);
                context.registerService(EventHandler.class.getName(), this,
                        properties);
            }
        } catch (final InvalidSyntaxException e) {
            ErlLogger.warn(e);
        }
    }

    private boolean contains(final ServiceReference[] refs,
            final ErlangEventHandler ref) {
        for (final ServiceReference r : refs) {
            if (r == ref) {
                return true;
            }
        }
        return false;
    }

    public IBackend getBackend() {
        return backend;
    }
}
