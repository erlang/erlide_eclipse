package org.erlide.util.event_tracer;

import org.eclipse.xtend2.lib.StringConcatenation;

@SuppressWarnings("all")
public class ErlideResetEvent extends ErlideEvent {
    public ErlideResetEvent() {
        super(System.currentTimeMillis());
    }

    @Override
    public String print() {
        final StringConcatenation _builder = new StringConcatenation();
        final long _timestamp = getTimestamp();
        _builder.append(_timestamp);
        _builder.append(" RESET");
        _builder.newLineIfNotEmpty();
        return _builder.toString();
    }
}
