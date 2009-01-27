/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.debug;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IValue;
import org.eclipse.debug.core.model.IVariable;
import org.erlide.core.ErlangPlugin;

import com.ericsson.otp.erlang.OtpErlangObject;

import erlang.ErlideDebug;

public class ErlangVariable extends ErlangDebugElement implements IVariable {
	private final String name;
	private final boolean subVariable;
	private final ErlangValue value;
	private final ErlangProcess process;
	private final int stackFrameNo;

	public ErlangVariable(final IDebugTarget target, final String name,
			final boolean subVariable, final OtpErlangObject value,
			final ErlangProcess process, String moduleName, final int stackFrameNo) {
		super(target);
		this.name = name;
		this.subVariable = subVariable;
		this.value = new ErlangValue(getDebugTarget(), name, value, process, moduleName);
		this.process = process;
		this.stackFrameNo = stackFrameNo;
	}

	public IValue getValue() throws DebugException {
		return value;
	}

	public String getName() throws DebugException {
		return name;
	}

	public String getReferenceTypeName() throws DebugException {
		if (value == null) {
			return null;
		}
		return value.getReferenceTypeName();
	}

	public boolean hasValueChanged() throws DebugException {
		return false;
	}

	public void setValue(final String expression) throws DebugException {
		if (subVariable) {
			throw new DebugException(new Status(IStatus.ERROR,
					ErlangPlugin.PLUGIN_ID, DebugException.NOT_SUPPORTED,
					"Can't set value of part of expression", null));
		}
		final ErlangDebugTarget edt = (ErlangDebugTarget) getDebugTarget();
		final String err = ErlideDebug.setVariableValue(edt.getBackend(), name,
				expression, stackFrameNo, process.getMeta());
		if (err != null) {
			throw new DebugException(new Status(IStatus.ERROR,
					ErlangPlugin.PLUGIN_ID,
					DebugException.TARGET_REQUEST_FAILED, "Bad expression",
					null));
		}

	}

	public void setValue(final IValue value) throws DebugException {
		setValue(value.toString());
	}

	public boolean supportsValueModification() {
		return !subVariable;
	}

	public boolean verifyValue(final String expression) throws DebugException {
		return true;
	}

	public boolean verifyValue(final IValue _value) throws DebugException {
		return verifyValue(_value.toString());
	}

}
