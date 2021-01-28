package org.erlide.engine.services.codeassist;

import java.util.Objects;

import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

@SuppressWarnings("all")
public class CompletionData {
    private final String displayString;

    private final String replacementString;

    private final int replacementOffset;

    private final int replacementLength;

    private final int cursorPosition;

    public String getDisplayString() {
        String _xifexpression = null;
        if (displayString == null) {
            _xifexpression = replacementString;
        } else {
            _xifexpression = displayString;
        }
        return _xifexpression;
    }

    public CompletionData(final String displayString, final String replacementString,
            final int replacementOffset, final int replacementLength,
            final int cursorPosition) {
        this.displayString = displayString;
        this.replacementString = replacementString;
        this.replacementOffset = replacementOffset;
        this.replacementLength = replacementLength;
        this.cursorPosition = cursorPosition;
    }

    @Override
    public int hashCode() {
        return Objects.hash(displayString, replacementString, replacementOffset, replacementLength, cursorPosition);
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
        final CompletionData other = (CompletionData) obj;
        if (!Objects.equals(displayString, other.displayString)) {
            return false;
        }
        if (!Objects.equals(replacementString, other.replacementString)) {
            return false;
        }
        if (other.replacementOffset != replacementOffset) {
            return false;
        }
        if (other.replacementLength != replacementLength) {
            return false;
        }
        if (other.cursorPosition != cursorPosition) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        final ToStringBuilder b = new ToStringBuilder(this);
        b.add("displayString", displayString);
        b.add("replacementString", replacementString);
        b.add("replacementOffset", replacementOffset);
        b.add("replacementLength", replacementLength);
        b.add("cursorPosition", cursorPosition);
        return b.toString();
    }

    public String getReplacementString() {
        return replacementString;
    }

    public int getReplacementOffset() {
        return replacementOffset;
    }

    public int getReplacementLength() {
        return replacementLength;
    }

    public int getCursorPosition() {
        return cursorPosition;
    }
}
