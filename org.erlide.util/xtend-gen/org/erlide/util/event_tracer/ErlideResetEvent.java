package org.erlide.util.event_tracer;

import org.eclipse.xtend2.lib.StringConcatenation;
import org.erlide.util.event_tracer.ErlideEvent;

@SuppressWarnings("all")
public class ErlideResetEvent extends ErlideEvent {
  public ErlideResetEvent() {
    super(System.currentTimeMillis());
  }
  
  public String print() {
    StringConcatenation _builder = new StringConcatenation();
    long _timestamp = this.getTimestamp();
    _builder.append(_timestamp, "");
    _builder.append(" RESET");
    _builder.newLineIfNotEmpty();
    return _builder.toString();
  }
}
