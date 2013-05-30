package org.erlide.util.event_tracer;

import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.erlide.util.IDisposable;
import org.erlide.util.event_tracer.ErlideCrashEvent;
import org.erlide.util.event_tracer.ErlideEvent;
import org.erlide.util.event_tracer.ErlideEventTracerHandler;
import org.erlide.util.event_tracer.ErlideOperationEvent;
import org.erlide.util.event_tracer.ErlideResetEvent;
import org.erlide.util.event_tracer.ErlideSessionEvent;
import org.erlide.util.event_tracer.ErlideStatusEvent;
import org.erlide.util.event_tracer.FileEventTracer;
import org.erlide.util.event_tracer.NullEventHandler;

@SuppressWarnings("all")
public class ErlideEventTracer implements IDisposable {
  private final static ErlideEventTracer instance = new Function0<ErlideEventTracer>() {
    public ErlideEventTracer apply() {
      ErlideEventTracer _erlideEventTracer = new ErlideEventTracer();
      return _erlideEventTracer;
    }
  }.apply();
  
  private final ErlideEventTracerHandler handler;
  
  public ErlideEventTracer() {
    final String tracerPath = System.getProperty("erlide.event_tracer");
    boolean _tripleEquals = (tracerPath == null);
    if (_tripleEquals) {
      NullEventHandler _nullEventHandler = new NullEventHandler();
      this.handler = _nullEventHandler;
    } else {
      FileEventTracer _fileEventTracer = new FileEventTracer(tracerPath);
      this.handler = _fileEventTracer;
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
  
  public void traceOperation(final String operation, final long duration) {
    ErlideOperationEvent _erlideOperationEvent = new ErlideOperationEvent(operation, duration);
    this.trace(_erlideOperationEvent);
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
