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

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IValue;
import org.eclipse.debug.core.model.IVariable;

import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlangVariable extends ErlangDebugElement implements IVariable {
	String name;
	ErlangValue value;

	public ErlangVariable(final IDebugTarget target, final String name,
			final OtpErlangObject value) {
		super(target);
		this.name = name;
		this.value = new ErlangValue(getDebugTarget(), name, value);
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
	}

	public void setValue(final IValue value) throws DebugException {
	}

	public boolean supportsValueModification() {
		return false;
	}

	public boolean verifyValue(final String expression) throws DebugException {
		return false;
	}

	public boolean verifyValue(final IValue value) throws DebugException {
		return false;
	}

}
