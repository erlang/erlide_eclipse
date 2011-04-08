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
public class Tuple3<X, Y, Z> implements Serializable {

    private static final long serialVersionUID = 2799440755988945843L;
    public X o1;
    public Y o2;
    public Z o3;

    public Tuple3(final X o1, final Y o2, final Z o3) {
        this.o1 = o1;
        this.o2 = o2;
        this.o3 = o3;
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
        buffer.append(']');
        return buffer.toString();
    }

    /**
     * Auto-generated code to deal with nulls.
     */
    @Override
    public int hashCode() {
        return Objects.hashCode(o1, o2, o3);
    }

    /**
     * Auto-generated code to deal with nulls.
     */
    @Override
    public boolean equals(final Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (!(obj instanceof Tuple3)) {
            return false;
        }
        @SuppressWarnings("rawtypes")
        final Tuple3 other = (Tuple3) obj;
        if (o1 == null) {
            if (other.o1 != null) {
                return false;
            }
        } else if (!o1.equals(other.o1)) {
            return false;
        }
        if (o2 == null) {
            if (other.o2 != null) {
                return false;
            }
        } else if (!o2.equals(other.o2)) {
            return false;
        }
        if (o3 == null) {
            if (other.o3 != null) {
                return false;
            }
        } else if (!o3.equals(other.o3)) {
            return false;
        }
        return true;
    }
}
