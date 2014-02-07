package org.erlide.util.event_tracer;

import org.eclipse.xtend2.lib.StringConcatenation;
import org.erlide.util.event_tracer.ErlideEvent;

@SuppressWarnings("all")
public class ErlideCrashEvent extends ErlideEvent {
  private final String backend;
  
  public ErlideCrashEvent(final String myBackend) {
    super(System.currentTimeMillis());
    this.backend = myBackend;
  }
  
  public String print() {
    StringConcatenation _builder = new StringConcatenation();
    long _timestamp = this.getTimestamp();
    _builder.append(_timestamp, "");
    _builder.append(" CRASH ");
    _builder.append(this.backend, "");
    _builder.newLineIfNotEmpty();
    return _builder.toString();
  }
}
