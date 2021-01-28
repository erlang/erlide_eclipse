package org.erlide.util.event_tracer;

import java.text.SimpleDateFormat;
import java.util.Date;

import org.eclipse.xtend2.lib.StringConcatenation;

@SuppressWarnings("all")
public class ErlideSessionEvent extends ErlideEvent {
    private final SimpleDateFormat formatter = new SimpleDateFormat(
            "yyyyMMdd-HHmmss-SSS");

    public final int workspace;

    public ErlideSessionEvent(final String aWorkspace) {
        super(System.currentTimeMillis());
        workspace = aWorkspace.hashCode();
    }

    @Override
    public String print() {
        final StringConcatenation _builder = new StringConcatenation();
        final long _timestamp = getTimestamp();
        _builder.append(_timestamp);
        _builder.append(" SESSION ");
        final long _timestamp_1 = getTimestamp();
        final Date _date = new Date(_timestamp_1);
        final String _format = formatter.format(_date);
        _builder.append(_format);
        _builder.newLineIfNotEmpty();
        return _builder.toString();
    }
}
