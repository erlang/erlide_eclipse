package org.erlide.engine.model.builder;

import java.util.Objects;

import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

@SuppressWarnings("all")
public class ProblemData0 {
    private final String tag;

    private final String message;

    private final int arity;

    public ProblemData0(final String tag, final String message, final int arity) {
        this.tag = tag;
        this.message = message;
        this.arity = arity;
    }

    @Override
    public int hashCode() {
        return Objects.hash(tag, message, arity);
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
        if (!Objects.equals(tag, other.tag)) {
            return false;
        }
        if (!Objects.equals(message, other.message)) {
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
