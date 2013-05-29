package org.erlide.util;

import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.erlide.util.ErlideEvent;

@SuppressWarnings("all")
public class ErlideCrashEvent extends ErlideEvent {
  private final String backend;
  
  public ErlideCrashEvent(final String myBackend) {
    super(new Function0<Long>() {
      public Long apply() {
        long _currentTimeMillis = System.currentTimeMillis();
        return _currentTimeMillis;
      }
    }.apply());
    this.backend = myBackend;
  }
}
