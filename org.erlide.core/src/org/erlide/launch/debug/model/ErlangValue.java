/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.launch.debug.model;

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IValue;
import org.eclipse.debug.core.model.IVariable;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangFloat;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlangValue extends ErlangDebugElement implements IValue {
    protected OtpErlangObject value;
    protected String varName; // to use with getVariables
    protected final ErlangProcess process;
    protected String moduleName;

    public ErlangValue(final IDebugTarget target, final String varName,
            final OtpErlangObject value, final ErlangProcess process,
            final String moduleName) {
        super(target);
        this.value = value;
        this.varName = varName;
        this.process = process;
        this.moduleName = moduleName;
    }

    @Override
    public String getReferenceTypeName() throws DebugException {
        if (value instanceof OtpErlangString) {
            return "string";
        } else if (value instanceof OtpErlangAtom) {
            return "atom";
        } else if (value instanceof OtpErlangList) {
            return "list";
        } else if (value instanceof OtpErlangTuple) {
            return "tuple";
        } else if (value instanceof OtpErlangPid) {
            return "pid";
        } else if (value instanceof OtpErlangLong) {
            return "integer";
        } else if (value instanceof OtpErlangFloat) {
            return "float";
        } else if (value instanceof OtpErlangBinary) {
            return "binary";
        } else {
            return "term";
        }
    }

    @Override
    public String getValueString() throws DebugException {
        return value.toString();
    }

    @Override
    public boolean isAllocated() throws DebugException {
        return true;
    }

    @Override
    public IVariable[] getVariables() throws DebugException {
        return null;
    }

    @Override
    public boolean hasVariables() throws DebugException {
        return false;
    }

    /**
     * The string rep. of the erlang value must be returned, it's used by
     * {@link ErlangVariable#setValue(IValue)}
     */
    @Override
    public String toString() {
        return value.toString();
    }
}
