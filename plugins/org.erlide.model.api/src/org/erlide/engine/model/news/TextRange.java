package org.erlide.engine.model.news;

import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

@Data
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
    @Pure
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + offset;
        return prime * result + length;
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
    @Pure
    public String toString() {
        final ToStringBuilder b = new ToStringBuilder(this);
        b.add("offset", offset);
        b.add("length", length);
        return b.toString();
    }

    @Pure
    public int getOffset() {
        return offset;
    }

    @Pure
    public int getLength() {
        return length;
    }
}
