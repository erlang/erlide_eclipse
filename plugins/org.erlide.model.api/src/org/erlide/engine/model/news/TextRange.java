package org.erlide.engine.model.news;

import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

@SuppressWarnings("all")
public class TextRange {
    private final int offset;

    private final int length;

    public TextRange(final int offset, final int length) {
        super();
        this.offset = offset;
        this.length = length;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + offset;
        return prime * result + length;
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
        final TextRange other = (TextRange) obj;
        if (other.offset != offset) {
            return false;
        }
        if (other.length != length) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        final ToStringBuilder b = new ToStringBuilder(this);
        b.add("offset", offset);
        b.add("length", length);
        return b.toString();
    }

    public int getOffset() {
        return offset;
    }

    public int getLength() {
        return length;
    }
}
