package org.erlide.util.event_tracer

import org.erlide.util.IDisposable

class ErlideEventTracer implements IDisposable {

    val static instance = new ErlideEventTracer
    val ErlideEventTracerHandler handler

    new() {
        val String tracerPath = System.getProperty("erlide.event_tracer");
        if (tracerPath === null) {
            handler = new NullEventHandler
        } else {
            handler = new ErlideEventTracerHandler(tracerPath)
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

    def traceOperationStart(String operation, Object id) {
        trace(new ErlideOperationStartEvent(operation, Integer.toHexString(System.identityHashCode(id))))
    }

    def traceOperationEnd(String operation, Object id) {
        trace(new ErlideOperationEndEvent(operation, Integer.toHexString(System.identityHashCode(id))))
    }

    def traceOperationStart(Object id) {
        traceOperationStart(id.class.simpleName, id)
    }

    def traceOperationEnd(Object id) {
        traceOperationEnd(id.class.simpleName, id)
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

    new() {
        super(null)
    }

    override handle(ErlideEvent event) {
    }

    override dispose() {
    }

}
