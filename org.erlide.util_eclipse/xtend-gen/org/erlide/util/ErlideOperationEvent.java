package org.erlide.util;

import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.erlide.util.ErlideEvent;

@SuppressWarnings("all")
public class ErlideOperationEvent extends ErlideEvent {
  private final String operation;
  
  private final long duration;
  
  public ErlideOperationEvent(final String myOperation, final long myDuration) {
    super(new Function0<Long>() {
      public Long apply() {
        long _currentTimeMillis = System.currentTimeMillis();
        return _currentTimeMillis;
      }
    }.apply());
    this.operation = myOperation;
    this.duration = myDuration;
  }
}
