package org.erlide.util

import java.net.Inet4Address
import org.eclipse.core.resources.ResourcesPlugin
import org.osgi.framework.BundleActivator
import org.osgi.framework.BundleContext
import org.osgi.service.event.Event
import org.osgi.service.event.EventAdmin
import org.osgi.util.tracker.ServiceTracker
import org.osgi.service.event.EventHandler
import java.util.Dictionary
import java.util.Hashtable
import org.osgi.service.event.EventConstants

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

@Data
abstract class ErlideEvent {
    String user = System::getProperty("user.name")
    String machine = Inet4Address::localHost.canonicalHostName
    String workspace = ResourcesPlugin::workspace.root.location.toPortableString
    val long timestamp
}

class ErlideSessionEvent extends ErlideEvent {
    new() {
        super(System::currentTimeMillis)
    }
}

class ErlideResetEvent extends ErlideEvent {
    new() {
        super(System::currentTimeMillis)
    }
}

class ErlideCrashEvent extends ErlideEvent {
    val String backend

    new(String myBackend) {
        super(System::currentTimeMillis)
        backend = myBackend
    }
}

class ErlideOperationEvent extends ErlideEvent {
    val String operation
    val long duration

    new(String myOperation, long myDuration) {
        super(System::currentTimeMillis)
        operation = myOperation
        duration = myDuration
    }
}

class ErlideStatusEvent extends ErlideEvent {
    val Object status

    new(Object myStatus) {
        super(System::currentTimeMillis)
        status = myStatus
    }
}
