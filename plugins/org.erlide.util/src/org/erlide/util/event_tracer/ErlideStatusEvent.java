package org.erlide.util.event_tracer;

import org.eclipse.xtend2.lib.StringConcatenation;

@SuppressWarnings("all")
public class ErlideStatusEvent extends ErlideEvent {
    private final Object status;

    public ErlideStatusEvent(final Object myStatus) {
        super(System.currentTimeMillis());
        status = myStatus;
    }

    @Override
    public String print() {
        final StringConcatenation _builder = new StringConcatenation();
        final long _timestamp = getTimestamp();
        _builder.append(_timestamp);
        _builder.append(" STATUS ");
        final String _string = status.toString();
        _builder.append(_string);
        _builder.newLineIfNotEmpty();
        return _builder.toString();
    }
}
