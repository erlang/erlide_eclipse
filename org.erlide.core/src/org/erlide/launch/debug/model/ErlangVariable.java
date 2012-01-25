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

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IValue;
import org.eclipse.debug.core.model.IVariable;
import org.erlide.core.ErlangCore;
import org.erlide.launch.debug.ErlideDebug;

import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlangVariable extends ErlangDebugElement implements IVariable {
    private final String name;
    private final boolean subVariable;
    private final ErlangValue value;
    private final ErlangProcess process;
    private final int stackFrameNo;

    public ErlangVariable(final IDebugTarget target, final String name,
            final boolean subVariable, final OtpErlangObject value,
            final ErlangProcess process, final String moduleName,
            final int stackFrameNo) {
        super(target);
        this.name = name;
        this.subVariable = subVariable;
        this.value = createErlangValue(name, value, process, moduleName);
        this.process = process;
        this.stackFrameNo = stackFrameNo;
    }

    private ErlangValue createErlangValue(final String aname,
            final OtpErlangObject avalue, final ErlangProcess aprocess,
            final String moduleName) {
        if (avalue instanceof OtpErlangList || avalue instanceof OtpErlangTuple
                || avalue instanceof OtpErlangBinary
                || avalue instanceof OtpErlangString) {
            return new IndexedErlangValue(getDebugTarget(), aname, avalue,
                    aprocess, moduleName);
        } else {
            return new ErlangValue(getDebugTarget(), aname, avalue, aprocess,
                    moduleName);
        }
    }

    @Override
    public IValue getValue() throws DebugException {
        return value;
    }

    @Override
    public String getName() throws DebugException {
        return name;
    }

    @Override
    public String getReferenceTypeName() throws DebugException {
        if (value == null) {
            return null;
        }
        return value.getReferenceTypeName();
    }

    @Override
    public boolean hasValueChanged() throws DebugException {
        return false;
    }

    @Override
    public void setValue(final String expression) throws DebugException {
        if (subVariable) {
            throw new DebugException(new Status(IStatus.ERROR,
                    ErlangCore.PLUGIN_ID, DebugException.NOT_SUPPORTED,
                    "Can't set value of part of expression", null));
        }
        final ErlangDebugTarget edt = getErlangDebugTarget();
        final String err = ErlideDebug.setVariableValue(edt.getBackend(), name,
                expression, stackFrameNo, process.getMeta());
        if (err != null) {
            throw new DebugException(new Status(IStatus.ERROR,
                    ErlangCore.PLUGIN_ID, DebugException.TARGET_REQUEST_FAILED,
                    "Bad expression", null));
        }

    }

    @Override
    public void setValue(final IValue value) throws DebugException {
        setValue(value.toString());
    }

    @Override
    public boolean supportsValueModification() {
        return !subVariable;
    }

    @Override
    public boolean verifyValue(final String expression) throws DebugException {
        return true;
    }

    @Override
    public boolean verifyValue(final IValue aValue) throws DebugException {
        return verifyValue(aValue.toString());
    }

}
