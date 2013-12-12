package org.erlide.util.event_tracer;

import org.erlide.util.event_tracer.ErlideEvent;

@SuppressWarnings("all")
public abstract class ErlideOperationEvent extends ErlideEvent {
  protected final String operation;
  
  protected final String id;
  
  public ErlideOperationEvent(final String myOperation, final String myId) {
    super(System.currentTimeMillis());
    this.operation = myOperation;
    this.id = myId;
  }
}
