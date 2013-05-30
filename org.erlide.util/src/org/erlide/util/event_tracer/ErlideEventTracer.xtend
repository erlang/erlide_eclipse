package org.erlide.util.event_tracer

import org.erlide.util.event_tracer.ErlideCrashEvent
import org.erlide.util.event_tracer.ErlideEvent
import org.erlide.util.event_tracer.ErlideOperationEvent
import org.erlide.util.event_tracer.ErlideResetEvent
import org.erlide.util.event_tracer.ErlideSessionEvent
import org.erlide.util.event_tracer.ErlideStatusEvent
import org.erlide.util.event_tracer.ErlideEventTracerHandler
import org.erlide.util.IDisposable

class ErlideEventTracer implements IDisposable {

    val static instance = new ErlideEventTracer
    val ErlideEventTracerHandler handler

    new() {
        val String tracerPath = System::getProperty("erlide.event_tracer");
        if (tracerPath === null) {
            handler = new NullEventHandler
        } else {
            handler = new FileEventTracer(tracerPath)
        }
    }

    def traceSession(String workspace) {
        trace(new ErlideSessionEvent(workspace))
    }

    def traceReset() {
        trace(new ErlideResetEvent())
    }

    def traceCrash(String backend) {
        trace(new ErlideCrashEvent(backend))
    }

    def traceStatus(Object status) {
        trace(new ErlideStatusEvent(status))
    }

    def traceOperation(String operation, long duration) {
        trace(new ErlideOperationEvent(operation, duration))
    }

    def private void trace(ErlideEvent event) {
        handler.handle(event)
    }

    def static getInstance() {
        instance
    }

    override dispose() {
        handler.dispose
    }

}

class NullEventHandler extends ErlideEventTracerHandler {

    override handle(ErlideEvent event) {
    }

    override dispose() {
    }

}
