package org.erlide.util.event_tracer;

import org.eclipse.xtend2.lib.StringConcatenation;
import org.erlide.util.event_tracer.ErlideOperationEvent;

@SuppressWarnings("all")
public class ErlideOperationEndEvent extends ErlideOperationEvent {
  public ErlideOperationEndEvent(final String myOperation, final String myId) {
    super(myOperation, myId);
  }
  
  public String print() {
    StringConcatenation _builder = new StringConcatenation();
    long _timestamp = this.getTimestamp();
    _builder.append(_timestamp, "");
    _builder.append(" OP< ");
    _builder.append(this.id, "");
    _builder.append(" ");
    _builder.append(this.operation, "");
    _builder.newLineIfNotEmpty();
    return _builder.toString();
  }
}
