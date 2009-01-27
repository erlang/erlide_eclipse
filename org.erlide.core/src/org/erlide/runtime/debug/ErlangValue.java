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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IValue;
import org.eclipse.debug.core.model.IVariable;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlPreprocessorDef;
import org.erlide.core.erlang.IErlRecordDef;
import org.erlide.core.erlang.util.ModelUtils;
import org.erlide.jinterface.rpc.Tuple;
import org.erlide.runtime.backend.Backend;

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
	protected String moduleName;
	protected IErlRecordDef record; // set if this value is a record

	// FIXME JC Maybe we should use polymorphism for this one?

	// public ErlangValue(final IDebugTarget target, ErlangVariable variable,
	// final String stringValue) {
	// super(target);
	// value = new OtpErlangString(stringValue);
	// this.variable = variable;
	// }

	public ErlangValue(final IDebugTarget target, final String varName,
			final OtpErlangObject value, final ErlangProcess process,
			final String moduleName) {
		super(target);
		this.value = value;
		this.varName = varName;
		this.process = process;
		this.moduleName = moduleName;
		checkRecord(value);
	}

	private void checkRecord(final OtpErlangObject o) {
		record = null;
		if (o instanceof OtpErlangTuple) {
			final OtpErlangTuple t = (OtpErlangTuple) o;
			final OtpErlangObject h = t.elementAt(0);
			if (h instanceof OtpErlangAtom) {
				final OtpErlangAtom a = (OtpErlangAtom) h;
				final ErlangDebugTarget target = (ErlangDebugTarget) getDebugTarget();
				final Backend b = target.getBackend();
				final IErlPreprocessorDef pd = ModelUtils
						.findPreprocessorDef(b, target.getProjects(),
								moduleName, a.atomValue(),
								IErlElement.Kind.RECORD_DEF, "",
								new ArrayList<Tuple>());
				if (pd != null) {
					final IErlRecordDef r = (IErlRecordDef) pd;
					final List<String> fields = r.getFields();
					if (fields != null && fields.size() + 1 == t.arity()) {
						record = r;
					}
				}
			}
		}
	}

	public String getReferenceTypeName() throws DebugException {
		if (record != null) {
			return "record";
		} else if (value instanceof OtpErlangString) {
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
		if (record != null) {
			final StringBuilder b = new StringBuilder();
			final List<String> fields = record.getFields();
			final OtpErlangTuple t = (OtpErlangTuple) value;
			b.append(t.elementAt(0)).append("#{");
			for (int i = 0; i < fields.size(); i++) {
				b.append(fields.get(i)).append("=").append(
						t.elementAt(i + 1).toString()).append(", ");
			}
			b.setLength(b.length() - 2);
			b.append("}");
			return b.toString();
		}
		return value.toString();
	}

	public boolean isAllocated() throws DebugException {
		return false;
	}

	public IVariable[] getVariables() throws DebugException {
		int arity = getArity();
		if (arity != -1) {
			final int ofs = record != null ? 1 : 0;
			arity -= ofs;
			final IVariable[] result = new IVariable[arity];
			for (int i = 0; i < arity; ++i) {
				final String name = record != null ? record.getFields().get(i)
						: varName + ":" + i;
				result[i] = new ErlangVariable(getDebugTarget(), name, true,
						getElementAt(i), process, moduleName, -1);

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
			final int ofs = record != null ? 1 : 0;
			return t.elementAt(index + ofs);
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
