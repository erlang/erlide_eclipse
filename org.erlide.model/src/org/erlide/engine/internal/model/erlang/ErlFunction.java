/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.engine.internal.model.erlang;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.IParent;
import org.erlide.engine.model.erlang.ErlangFunction;
import org.erlide.engine.model.erlang.IErlComment;
import org.erlide.engine.model.erlang.IErlFunction;
import org.erlide.engine.model.erlang.IErlFunctionClause;
import org.erlide.engine.model.erlang.IErlTypespec;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;

import com.ericsson.otp.erlang.OtpErlangList;
import com.google.common.collect.Lists;

/**
 *
 * @author Vlad Dumitrescu
 */
public class ErlFunction extends ErlMember implements IErlFunction {

    private static final Collection<IErlComment> NO_COMMENTS = Lists.newArrayList();

    private final boolean fExported;
    private int arity;
    private final String head;
    private final List<String> parameters;
    private Collection<IErlComment> fComments = NO_COMMENTS;
    private IErlTypespec typespec;

    /**
     * @param parent
     * @param name
     * @param arity
     * @param head
     * @param comment
     */
    public ErlFunction(final IParent parent, final String name, final int arity,
            final String head, final boolean exported, final OtpErlangList parameters) {
        super(parent, name);
        this.arity = arity;
        this.head = head;
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
    public ErlElementKind getKind() {
        return ErlElementKind.FUNCTION;
    }

    @Override
    public int getArity() {
        return arity;
    }

    @Override
    public boolean isExported() {
        return fExported
                || ErlangEngine.getInstance().getModelUtilService().getModule(this)
                        .exportsAllFunctions();
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
    public Collection<IErlComment> getComments() {
        return fComments;
    }

    @Override
    public String getFunctionName() {
        return getName();
    }

    @Override
    public List<String> getParameters() {
        return parameters;
    }

    @Override
    public void setComments(final Collection<IErlComment> comments) {
        fComments = comments;
    }

    @Override
    public void setTypespec(final IErlTypespec spec) {
        if (isValidSpec(spec)) {
            typespec = spec;
        }
    }

    private boolean isValidSpec(final IErlTypespec spec) {
        return spec.getName().equals(getName()) && spec.getArity() == getArity();
    }

    @Override
    public IErlTypespec getTypespec() {
        return typespec;
    }

}
