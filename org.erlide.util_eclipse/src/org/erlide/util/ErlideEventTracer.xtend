package org.erlide.util

import java.util.Dictionary
import java.util.Hashtable
import org.osgi.framework.BundleActivator
import org.osgi.framework.BundleContext
import org.osgi.service.event.Event
import org.osgi.service.event.EventAdmin
import org.osgi.service.event.EventConstants
import org.osgi.service.event.EventHandler
import org.osgi.util.tracker.ServiceTracker

class ErlideEventTracer implements BundleActivator {

    val static public String ERLIDE_EVENT_TOPIC = "org/erlide/erlide_event"

    ServiceTracker<Object, Object> tracker
    val static instance = new ErlideEventTracer
    static boolean hasHandlers = false

    override start(BundleContext context) throws Exception {
        tracker = new ServiceTracker(context, typeof(EventAdmin).name, null)
        tracker.open
    }

    override stop(BundleContext context) throws Exception {
        if(tracker !== null) tracker.close
    }

    def static doStart(BundleContext context) {
        instance.start(context)
    }

    def static doStop(BundleContext context) {
        instance.stop(context)
    }

    def static traceSession() {
        instance.trace(new ErlideSessionEvent())
    }

    def static traceReset() {
        instance.trace(new ErlideResetEvent())
    }

    def static traceCrash(String backend) {
        instance.trace(new ErlideCrashEvent(backend))
    }

    def static traceStatus(Object status) {
        instance.trace(new ErlideStatusEvent(status))
    }

    def static traceOperation(String operation, long duration) {
        instance.trace(new ErlideOperationEvent(operation, duration))
    }

    def private void trace(ErlideEvent event) {
        if (tracker === null || !hasHandlers) {
            return
        }
        val EventAdmin ea = tracker.service as EventAdmin
        if (ea !== null) {
            val evt = new Event(ERLIDE_EVENT_TOPIC, newHashMap("event" -> event))
            ea.postEvent(evt)
        }
    }

    def static registerHandler(EventHandler handler, BundleContext context) {
        val Dictionary<String, Object> ht = new Hashtable()
        val String[] topics = #[ErlideEventTracer::ERLIDE_EVENT_TOPIC]
        ht.put(EventConstants::EVENT_TOPIC, topics)
        context.registerService(typeof(EventHandler).name, handler, ht)
        hasHandlers = true;
    }
}

