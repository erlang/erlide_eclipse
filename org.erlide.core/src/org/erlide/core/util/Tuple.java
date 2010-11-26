/*
 * Created on 24/09/2005
 */
package org.erlide.core.util;

import java.io.Serializable;

/**
 * Defines a tuple of some object, adding equals and hashCode operations
 * 
 * @author Fabio
 */
public class Tuple<X, Y> implements Serializable {

    private static final long serialVersionUID = 5812091735998675402L;
    public X o1;
    public Y o2;

    public Tuple(final X o1, final Y o2) {
        this.o1 = o1;
        this.o2 = o2;
    }

    public Tuple(final Tuple<X, Y> other) {
        this.o1 = other.o1;
        this.o2 = other.o2;
    }

    @Override
    public boolean equals(final Object obj) {
        if (!(obj instanceof Tuple)) {
            return false;
        }

        @SuppressWarnings("rawtypes")
        final Tuple t2 = (Tuple) obj;
        if (o1 == t2.o1 && o2 == t2.o2) { // all the same
            return true;
        }

        if (o1 == null && t2.o1 != null) {
            return false;
        }
        if (o2 == null && t2.o2 != null) {
            return false;
        }
        if (o1 != null && t2.o1 == null) {
            return false;
        }
        if (o2 != null && t2.o2 == null) {
            return false;
        }

        if (!o1.equals(t2.o1)) {
            return false;
        }
        if (!o2.equals(t2.o2)) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        if (o1 != null && o2 != null) {
            return o1.hashCode() * o2.hashCode();
        }
        if (o1 != null) {
            return o1.hashCode();
        }
        if (o2 != null) {
            return o2.hashCode();
        }
        return 7;
    }

    @Override
    public String toString() {
        final StringBuffer buffer = new StringBuffer();
        buffer.append("Tuple [");
        buffer.append(o1);
        buffer.append(" -- ");
        buffer.append(o2);
        buffer.append(']');
        return buffer.toString();
    }
}
