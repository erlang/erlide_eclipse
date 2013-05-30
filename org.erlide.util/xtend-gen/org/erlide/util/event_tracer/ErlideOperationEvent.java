package org.erlide.util.event_tracer;

import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.erlide.util.event_tracer.ErlideEvent;

@SuppressWarnings("all")
public abstract class ErlideOperationEvent extends ErlideEvent {
  protected final String operation;
  
  protected final String id;
  
  public ErlideOperationEvent(final String myOperation, final String myId) {
    super(new Function0<Long>() {
      public Long apply() {
        long _currentTimeMillis = System.currentTimeMillis();
        return _currentTimeMillis;
      }
    }.apply());
    this.operation = myOperation;
    this.id = myId;
  }
}
