package org.erlide.util.event_tracer;

import java.io.PrintWriter;

import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

@SuppressWarnings("all")
public abstract class ErlideEvent {
    private final long timestamp;

    public void print(final PrintWriter file) {
        if (file != null) {
            file.print(this.print());
        }
    }

    public abstract String print();

    public ErlideEvent(final long timestamp) {
        this.timestamp = timestamp;
    }

    @Override
    public int hashCode() {
        return 31 * 1 + (int) (timestamp ^ timestamp >>> 32);
    }

    @Override
    public boolean equals(final Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final ErlideEvent other = (ErlideEvent) obj;
        if (other.timestamp != timestamp) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        final ToStringBuilder b = new ToStringBuilder(this);
        b.add("timestamp", timestamp);
        return b.toString();
    }

    public long getTimestamp() {
        return timestamp;
    }
}
