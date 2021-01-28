package org.erlide.util.event_tracer;

import org.erlide.util.IDisposable;

@SuppressWarnings("all")
public class ErlideEventTracer implements IDisposable {
    private static final ErlideEventTracer instance = new ErlideEventTracer();

    private final ErlideEventTracerHandler handler;

    public ErlideEventTracer() {
        final String tracerPath = System.getProperty("erlide.event_tracer");
        if (tracerPath == null) {
            final NullEventHandler _nullEventHandler = new NullEventHandler();
            handler = _nullEventHandler;
        } else {
            final ErlideEventTracerHandler _erlideEventTracerHandler = new ErlideEventTracerHandler(
                    tracerPath);
            handler = _erlideEventTracerHandler;
        }
    }

    public void traceSession(final String workspace) {
        final ErlideSessionEvent _erlideSessionEvent = new ErlideSessionEvent(workspace);
        trace(_erlideSessionEvent);
    }

    public void traceReset() {
        final ErlideResetEvent _erlideResetEvent = new ErlideResetEvent();
        trace(_erlideResetEvent);
    }

    public void traceCrash(final String backend) {
        final ErlideCrashEvent _erlideCrashEvent = new ErlideCrashEvent(backend);
        trace(_erlideCrashEvent);
    }

    public void traceStatus(final Object status) {
        final ErlideStatusEvent _erlideStatusEvent = new ErlideStatusEvent(status);
        trace(_erlideStatusEvent);
    }

    public void traceOperationStart(final String operation, final Object id) {
        final String _hexString = Integer.toHexString(System.identityHashCode(id));
        final ErlideOperationStartEvent _erlideOperationStartEvent = new ErlideOperationStartEvent(
                operation, _hexString);
        trace(_erlideOperationStartEvent);
    }

    public void traceOperationEnd(final String operation, final Object id) {
        final String _hexString = Integer.toHexString(System.identityHashCode(id));
        final ErlideOperationEndEvent _erlideOperationEndEvent = new ErlideOperationEndEvent(
                operation, _hexString);
        trace(_erlideOperationEndEvent);
    }

    public void traceOperationStart(final Object id) {
        this.traceOperationStart(id.getClass().getSimpleName(), id);
    }

    public void traceOperationEnd(final Object id) {
        this.traceOperationEnd(id.getClass().getSimpleName(), id);
    }

    private void trace(final ErlideEvent event) {
        handler.handle(event);
    }

    public static ErlideEventTracer getInstance() {
        return ErlideEventTracer.instance;
    }

    @Override
    public void dispose() {
        handler.dispose();
    }
}
