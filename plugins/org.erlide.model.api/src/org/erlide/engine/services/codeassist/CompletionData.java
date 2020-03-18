package org.erlide.engine.services.codeassist;

import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

@Data
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
        super();
        this.displayString = displayString;
        this.replacementString = replacementString;
        this.replacementOffset = replacementOffset;
        this.replacementLength = replacementLength;
        this.cursorPosition = cursorPosition;
    }

    @Override
    @Pure
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + (displayString == null ? 0 : displayString.hashCode());
        result = prime * result
                + (replacementString == null ? 0 : replacementString.hashCode());
        result = prime * result + replacementOffset;
        result = prime * result + replacementLength;
        return prime * result + cursorPosition;
    }

    @Override
    @Pure
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
        if (displayString == null) {
            if (other.displayString != null) {
                return false;
            }
        } else if (!displayString.equals(other.displayString)) {
            return false;
        }
        if (replacementString == null) {
            if (other.replacementString != null) {
                return false;
            }
        } else if (!replacementString.equals(other.replacementString)) {
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
    @Pure
    public String toString() {
        final ToStringBuilder b = new ToStringBuilder(this);
        b.add("displayString", displayString);
        b.add("replacementString", replacementString);
        b.add("replacementOffset", replacementOffset);
        b.add("replacementLength", replacementLength);
        b.add("cursorPosition", cursorPosition);
        return b.toString();
    }

    @Pure
    public String getReplacementString() {
        return replacementString;
    }

    @Pure
    public int getReplacementOffset() {
        return replacementOffset;
    }

    @Pure
    public int getReplacementLength() {
        return replacementLength;
    }

    @Pure
    public int getCursorPosition() {
        return cursorPosition;
    }
}
