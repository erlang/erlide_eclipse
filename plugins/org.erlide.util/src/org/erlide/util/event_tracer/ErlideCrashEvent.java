package org.erlide.util.event_tracer;

import org.eclipse.xtend2.lib.StringConcatenation;

@SuppressWarnings("all")
public class ErlideCrashEvent extends ErlideEvent {
    private final String backend;

    public ErlideCrashEvent(final String myBackend) {
        super(System.currentTimeMillis());
        backend = myBackend;
    }

    @Override
    public String print() {
        final StringConcatenation _builder = new StringConcatenation();
        final long _timestamp = getTimestamp();
        _builder.append(_timestamp);
        _builder.append(" CRASH ");
        _builder.append(backend);
        _builder.newLineIfNotEmpty();
        return _builder.toString();
    }
}
