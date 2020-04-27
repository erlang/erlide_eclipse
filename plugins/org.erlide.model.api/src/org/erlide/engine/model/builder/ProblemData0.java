package org.erlide.engine.model.builder;

import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

@SuppressWarnings("all")
public class ProblemData0 {
    private final String tag;

    private final String message;

    private final int arity;

    public ProblemData0(final String tag, final String message, final int arity) {
        super();
        this.tag = tag;
        this.message = message;
        this.arity = arity;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + (tag == null ? 0 : tag.hashCode());
        result = prime * result + (message == null ? 0 : message.hashCode());
        return prime * result + arity;
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
        final ProblemData0 other = (ProblemData0) obj;
        if (tag == null) {
            if (other.tag != null) {
                return false;
            }
        } else if (!tag.equals(other.tag)) {
            return false;
        }
        if (message == null) {
            if (other.message != null) {
                return false;
            }
        } else if (!message.equals(other.message)) {
            return false;
        }
        if (other.arity != arity) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        final ToStringBuilder b = new ToStringBuilder(this);
        b.add("tag", tag);
        b.add("message", message);
        b.add("arity", arity);
        return b.toString();
    }

    public String getTag() {
        return tag;
    }

    public String getMessage() {
        return message;
    }

    public int getArity() {
        return arity;
    }
}
