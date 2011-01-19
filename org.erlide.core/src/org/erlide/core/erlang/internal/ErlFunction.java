/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.erlang.internal;

import java.util.ArrayList;
import java.util.List;

import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlFunctionClause;
import org.erlide.core.erlang.IParent;
import org.erlide.core.erlang.util.ErlangFunction;

import com.ericsson.otp.erlang.OtpErlangList;

/**
 * 
 * @author Vlad Dumitrescu
 */
public class ErlFunction extends ErlMember implements IErlFunction, IParent {

    private final boolean fExported;

    private int arity;

    private final String head;

    private final String fComment;

    private final List<String> parameters;

    /**
     * @param parent
     * @param name
     * @param arity
     * @param head
     * @param comment
     */
    protected ErlFunction(final IParent parent, final String name,
            final int arity, final String head, final String comment,
            final boolean exported, final OtpErlangList parameters) {
        super(parent, name);
        this.arity = arity;
        this.head = head;
        fComment = comment;
        fExported = exported;
        this.parameters = ErlFunctionClause.getParameters(parameters);
    }

    public List<IErlFunctionClause> getClauses() {
        final ArrayList<IErlFunctionClause> fc = new ArrayList<IErlFunctionClause>();
        try {
            for (final IErlElement el : getChildren()) {
                if (el instanceof IErlFunctionClause) {
                    fc.add((IErlFunctionClause) el);
                }
            }
        } catch (final ErlModelException e) {
        }
        return fc;
    }

    public Kind getKind() {
        return Kind.FUNCTION;
    }

    public int getArity() {
        return arity;
    }

    public boolean isExported() {
        return fExported;
    }

    public void setArity(final int i) {
        arity = i;
    }

    @Override
    public String toString() {
        final StringBuilder b = new StringBuilder();
        b.append(getName()).append('/').append(getArity());
        if (head != null && head.length() != 0) {
            b.append("  ").append(head);
        }
        return b.toString();
    }

    public ErlangFunction getFunction() {
        return new ErlangFunction(getName(), getArity());
    }

    @Override
    public int hashCode() {
        final int PRIME = 31;
        int result = super.hashCode();
        result = PRIME * result + arity;
        return result;
    }

    @Override
    public boolean equals(final Object obj) {
        if (this == obj) {
            return true;
        }
        if (!super.equals(obj)) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final ErlFunction other = (ErlFunction) obj;
        if (arity != other.arity) {
            return false;
        }
        return true;
    }

    public String getNameWithArity() {
        return toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.erlide.core.erlang.IErlFunction#getNameWithParameters()
     */
    public String getNameWithParameters() {
        return getNameWithParameters(getName(), getArity());
    }

    public static String getNameWithParameters(final String name,
            final int arity) {
        final StringBuilder b = new StringBuilder();
        b.append(name).append('(');
        for (int i = 0; i < arity; i++) {
            b.append('_');
            if (i < arity - 1) {
                b.append(", ");
            }
        }
        b.append(')');
        return b.toString();
    }

    public String getHead() {
        return head;
    }

    public String getComment() {
        return fComment;
    }

    public String getFunctionName() {
        return getName();
    }

    public List<String> getParameters() {
        return parameters;
    }

}
