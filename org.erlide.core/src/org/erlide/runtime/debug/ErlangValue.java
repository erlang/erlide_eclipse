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
	private final ErlangProcess process;

	// public ErlangValue(final IDebugTarget target, ErlangVariable variable,
	// final String stringValue) {
	// super(target);
	// value = new OtpErlangString(stringValue);
	// this.variable = variable;
	// }

	public ErlangValue(final IDebugTarget target, final String varName,
			final OtpErlangObject value, final ErlangProcess process) {
		super(target);
		this.value = value;
		this.varName = varName;
		this.process = process;
	}

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

	public String getValueString() throws DebugException {
		return value.toString();
	}

	public boolean isAllocated() throws DebugException {
		return false;
	}

	public IVariable[] getVariables() throws DebugException {
		final int arity = getArity();
		if (arity != -1) {
			final IVariable[] result = new IVariable[arity];
			for (int i = 0; i < arity; ++i) {
				result[i] = new ErlangVariable(getDebugTarget(), varName + ":"
						+ i, true, getElementAt(i), process, -1);
			}
			return result;
		}
		return null;
	}

	public boolean hasVariables() throws DebugException {
		return getArity() != -1;
	}

	protected OtpErlangObject getElementAt(final int index) {
		if (value instanceof OtpErlangTuple) {
			final OtpErlangTuple t = (OtpErlangTuple) value;
			return t.elementAt(index);
		} else if (value instanceof OtpErlangList) {
			final OtpErlangList l = (OtpErlangList) value;
			return l.elementAt(index);
		}
		return null;
	}

	protected int getArity() {
		if (value instanceof OtpErlangTuple) {
			final OtpErlangTuple t = (OtpErlangTuple) value;
			return t.arity();
		} else if (value instanceof OtpErlangList) {
			final OtpErlangList l = (OtpErlangList) value;
			return l.arity();
		} else {
			return -1;
		}

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
