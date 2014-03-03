package org.erlide.util.event_tracer;

import org.erlide.util.IDisposable;
import org.erlide.util.event_tracer.ErlideCrashEvent;
import org.erlide.util.event_tracer.ErlideEvent;
import org.erlide.util.event_tracer.ErlideEventTracerHandler;
import org.erlide.util.event_tracer.ErlideOperationEndEvent;
import org.erlide.util.event_tracer.ErlideOperationStartEvent;
import org.erlide.util.event_tracer.ErlideResetEvent;
import org.erlide.util.event_tracer.ErlideSessionEvent;
import org.erlide.util.event_tracer.ErlideStatusEvent;
import org.erlide.util.event_tracer.NullEventHandler;

@SuppressWarnings("all")
public class ErlideEventTracer implements IDisposable {
  private final static ErlideEventTracer instance = new ErlideEventTracer();
  
  private final ErlideEventTracerHandler handler;
  
  public ErlideEventTracer() {
    final String tracerPath = System.getProperty("erlide.event_tracer");
    boolean _tripleEquals = (tracerPath == null);
    if (_tripleEquals) {
      NullEventHandler _nullEventHandler = new NullEventHandler();
      this.handler = _nullEventHandler;
    } else {
      ErlideEventTracerHandler _erlideEventTracerHandler = new ErlideEventTracerHandler(tracerPath);
      this.handler = _erlideEventTracerHandler;
    }
  }
  
  public void traceSession(final String workspace) {
    ErlideSessionEvent _erlideSessionEvent = new ErlideSessionEvent(workspace);
    this.trace(_erlideSessionEvent);
  }
  
  public void traceReset() {
    ErlideResetEvent _erlideResetEvent = new ErlideResetEvent();
    this.trace(_erlideResetEvent);
  }
  
  public void traceCrash(final String backend) {
    ErlideCrashEvent _erlideCrashEvent = new ErlideCrashEvent(backend);
    this.trace(_erlideCrashEvent);
  }
  
  public void traceStatus(final Object status) {
    ErlideStatusEvent _erlideStatusEvent = new ErlideStatusEvent(status);
    this.trace(_erlideStatusEvent);
  }
  
  public void traceOperationStart(final String operation, final Object id) {
    int _identityHashCode = System.identityHashCode(id);
    String _hexString = Integer.toHexString(_identityHashCode);
    ErlideOperationStartEvent _erlideOperationStartEvent = new ErlideOperationStartEvent(operation, _hexString);
    this.trace(_erlideOperationStartEvent);
  }
  
  public void traceOperationEnd(final String operation, final Object id) {
    int _identityHashCode = System.identityHashCode(id);
    String _hexString = Integer.toHexString(_identityHashCode);
    ErlideOperationEndEvent _erlideOperationEndEvent = new ErlideOperationEndEvent(operation, _hexString);
    this.trace(_erlideOperationEndEvent);
  }
  
  public void traceOperationStart(final Object id) {
    Class<?> _class = id.getClass();
    String _simpleName = _class.getSimpleName();
    this.traceOperationStart(_simpleName, id);
  }
  
  public void traceOperationEnd(final Object id) {
    Class<?> _class = id.getClass();
    String _simpleName = _class.getSimpleName();
    this.traceOperationEnd(_simpleName, id);
  }
  
  private void trace(final ErlideEvent event) {
    this.handler.handle(event);
  }
  
  public static ErlideEventTracer getInstance() {
    return ErlideEventTracer.instance;
  }
  
  public void dispose() {
    this.handler.dispose();
  }
}
