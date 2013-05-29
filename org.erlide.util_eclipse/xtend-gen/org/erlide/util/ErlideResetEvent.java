package org.erlide.util;

import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.erlide.util.ErlideEvent;

@SuppressWarnings("all")
public class ErlideResetEvent extends ErlideEvent {
  public ErlideResetEvent() {
    super(new Function0<Long>() {
      public Long apply() {
        long _currentTimeMillis = System.currentTimeMillis();
        return _currentTimeMillis;
      }
    }.apply());
  }
}
