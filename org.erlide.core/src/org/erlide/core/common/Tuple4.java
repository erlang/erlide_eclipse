/*
 * Created on Mar 19, 2006
 */
package org.erlide.core.common;

import java.io.Serializable;

import com.google.common.base.Objects;

/**
 * Defines a tuple of some object, adding equals and hashCode operations
 * 
 * @author Fabio
 */
public class Tuple4<X, Y, Z, T> implements Serializable {

    private static final long serialVersionUID = 7613168622304303958L;
    public X o1;
    public Y o2;
    public Z o3;
    public T o4;

    public Tuple4(final X o1, final Y o2, final Z o3, final T o4) {
        this.o1 = o1;
        this.o2 = o2;
        this.o3 = o3;
        this.o4 = o4;
    }

    @Override
    public boolean equals(final Object obj) {
        if (!(obj instanceof Tuple4)) {
            return false;
        }

        @SuppressWarnings("rawtypes")
        final Tuple4 t2 = (Tuple4) obj;
        if (!o1.equals(t2.o1)) {
            return false;
        }
        if (!o2.equals(t2.o2)) {
            return false;
        }
        if (!o3.equals(t2.o3)) {
            return false;
        }
        if (!o4.equals(t2.o4)) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(o1, o2, o3, o4);
    }

    @Override
    public String toString() {
        final StringBuffer buffer = new StringBuffer();
        buffer.append("Tuple [");
        buffer.append(o1);
        buffer.append(" -- ");
        buffer.append(o2);
        buffer.append(" -- ");
        buffer.append(o3);
        buffer.append(" -- ");
        buffer.append(o4);
        buffer.append(']');
        return buffer.toString();
    }
}
