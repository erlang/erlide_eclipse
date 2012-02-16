/*
 * Created on 24/09/2005
 */
package org.erlide.utils;

import java.io.Serializable;

import com.google.common.base.Objects;

/**
 * Defines a tuple of some object, adding equals and hashCode operations
 * 
 * @author Fabio
 */
public class Tuple<X, Y> implements Serializable {

    private static final long serialVersionUID = 5812091735998675402L;
    public X first;
    public Y second;

    public Tuple(final X o1, final Y o2) {
        this.first = o1;
        this.second = o2;
    }

    public Tuple(final Tuple<X, Y> other) {
        this.first = other.first;
        this.second = other.second;
    }

    @Override
    public boolean equals(final Object obj) {
        if (!(obj instanceof Tuple)) {
            return false;
        }

        @SuppressWarnings("rawtypes")
        final Tuple t2 = (Tuple) obj;
        if (first == t2.first && second == t2.second) { // all the same
            return true;
        }

        if (first == null && t2.first != null) {
            return false;
        }
        if (second == null && t2.second != null) {
            return false;
        }
        if (first != null && t2.first == null) {
            return false;
        }
        if (second != null && t2.second == null) {
            return false;
        }

        if (!first.equals(t2.first)) {
            return false;
        }
        if (!second.equals(t2.second)) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(first, second);
    }

    @Override
    public String toString() {
        final StringBuffer buffer = new StringBuffer();
        buffer.append("Tuple[");
        buffer.append(first);
        buffer.append(" -- ");
        buffer.append(second);
        buffer.append(']');
        return buffer.toString();
    }
}
