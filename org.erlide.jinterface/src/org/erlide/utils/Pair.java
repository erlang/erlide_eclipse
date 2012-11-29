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
public class Pair<X, Y> implements Serializable {

    private static final long serialVersionUID = 5812091735998675402L;
    private final X key;
    private final Y value;

    public Pair(final X o1, final Y o2) {
        key = o1;
        value = o2;
    }

    public Pair(final Pair<X, Y> other) {
        key = other.getKey();
        value = other.getValue();
    }

    @Override
    public boolean equals(final Object obj) {
        if (!(obj instanceof Pair)) {
            return false;
        }

        @SuppressWarnings("rawtypes")
        final Pair t2 = (Pair) obj;
        if (getKey() == t2.getKey() && getValue() == t2.getValue()) { // all the
                                                                      // same
            return true;
        }

        if (getKey() == null && t2.getKey() != null) {
            return false;
        }
        if (getValue() == null && t2.getValue() != null) {
            return false;
        }
        if (getKey() != null && t2.getKey() == null) {
            return false;
        }
        if (getValue() != null && t2.getValue() == null) {
            return false;
        }

        if (!getKey().equals(t2.getKey())) {
            return false;
        }
        if (!getValue().equals(t2.getValue())) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(getKey(), getValue());
    }

    @Override
    public String toString() {
        final StringBuffer buffer = new StringBuffer();
        buffer.append("Pair[");
        buffer.append(getKey());
        buffer.append(" -- ");
        buffer.append(getValue());
        buffer.append(']');
        return buffer.toString();
    }

    public Y getValue() {
        return value;
    }

    public X getKey() {
        return key;
    }

}
