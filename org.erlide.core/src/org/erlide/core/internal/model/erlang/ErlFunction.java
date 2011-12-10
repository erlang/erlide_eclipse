/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.internal.model.erlang;

import java.util.ArrayList;
import java.util.List;

import org.erlide.core.internal.model.root.ErlMember;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlFunctionClause;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IParent;
import org.erlide.core.model.util.ErlangFunction;

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
    public ErlFunction(final IParent parent, final String name,
            final int arity, final String head, final String comment,
            final boolean exported, final OtpErlangList parameters) {
        super(parent, name);
        this.arity = arity;
        this.head = head;
        fComment = comment;
        fExported = exported;
        this.parameters = ErlFunctionClause.getParameters(parameters);
    }

    @Override
    public List<IErlFunctionClause> getClauses() {
        final ArrayList<IErlFunctionClause> fc = new ArrayList<IErlFunctionClause>();
        synchronized (getModelLock()) {
            for (final IErlElement el : internalGetChildren()) {
                if (el instanceof IErlFunctionClause) {
                    fc.add((IErlFunctionClause) el);
                }
            }
        }
        return fc;
    }

    @Override
    public Kind getKind() {
        return Kind.FUNCTION;
    }

    @Override
    public int getArity() {
        return arity;
    }

    @Override
    public boolean isExported() {
        return fExported || getModule().exportsAllFunctions();
    }

    public void setArity(final int i) {
        arity = i;
    }

    @Override
    public String toString() {
        final StringBuilder b = getNameWithArityBuilder();
        if (head != null && head.length() != 0) {
            b.append("  ").append(head);
        }
        return b.toString();
    }

    @Override
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

    @Override
    public String getNameWithArity() {
        final StringBuilder b = getNameWithArityBuilder();
        return b.toString();
    }

    private StringBuilder getNameWithArityBuilder() {
        final StringBuilder b = new StringBuilder();
        b.append(getName()).append('/').append(getArity());
        return b;
    }

    @Override
    public String getNameWithParameters() {
        return ErlangFunction.getNameWithParameters(getName(), getArity());
    }

    @Override
    public String getHead() {
        return head;
    }

    @Override
    public String getComment() {
        return fComment;
    }

    @Override
    public String getFunctionName() {
        return getName();
    }

    @Override
    public List<String> getParameters() {
        return parameters;
    }

}
