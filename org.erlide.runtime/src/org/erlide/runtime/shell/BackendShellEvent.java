package org.erlide.runtime.shell;

public class BackendShellEvent {

    private final int offset;
    private final int removedLength;
    private final String text;

    public BackendShellEvent(final int offset, final int removedLength, final String text) {
        this.offset = offset;
        this.removedLength = removedLength;
        this.text = text;
    }

    public int getOffset() {
        return offset;
    }

    public String getText() {
        return text;
    }

    public int getRemovedLength() {
        return removedLength;
    }

}
