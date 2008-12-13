package org.erlide.core.util;

import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.internal.ErlFunction;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlangFunction {

	public String name;

	public int arity;

	/**
	 * @param name
	 * @param arity
	 */
	public ErlangFunction(final String name, final int arity) {
		super();
		this.name = name;
		this.arity = arity;
	}

	public ErlangFunction(final OtpErlangTuple tuple) {
		OtpErlangAtom a;
		OtpErlangLong l;
		if (tuple.arity() == 2) {
			a = (OtpErlangAtom) tuple.elementAt(0);
			l = (OtpErlangLong) tuple.elementAt(1);
		} else {
			a = (OtpErlangAtom) tuple.elementAt(2);
			l = (OtpErlangLong) tuple.elementAt(3);
		}
		name = a.atomValue();
		try {
			arity = l.intValue();
		} catch (final OtpErlangRangeException e) {
			arity = 0;
		}
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj instanceof String) {
			return toString().equals(obj);
		}
		if (obj instanceof OtpErlangTuple) {
			return new ErlangFunction((OtpErlangTuple) obj).equals(this);
		}
		if (obj instanceof ErlangFunction) {
			final ErlangFunction f = (ErlangFunction) obj;
			if (f.name.equals(name)) {
				return f.arity == arity || f.arity == IErlModel.UNKNOWN_ARITY
						|| arity == IErlModel.UNKNOWN_ARITY;
			}
		}
		return false;
	}

	@Override
	public int hashCode() {
		return arity * 123 + name.hashCode();
	}

	@Override
	public String toString() {
		return name + "/" + Integer.toString(arity);
	}

	public String getNameWithArity() {
		return toString();
	}

	public String getNameWithParameters() {
		return ErlFunction.getNameWithParameters(name, arity);
	}

}
