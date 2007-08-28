package org.erlide.core.util;

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
	public ErlangFunction(String name, int arity) {
		super();
		this.name = name;
		this.arity = arity;
	}

	public ErlangFunction(OtpErlangTuple tuple) throws OtpErlangRangeException {
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
		arity = l.intValue();
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof String) {
			return toString().equals(obj);
		}
		if (obj instanceof OtpErlangTuple) {
			try {
				return new ErlangFunction((OtpErlangTuple) obj).equals(this);
			} catch (final OtpErlangRangeException e) {
				return false;
			}
		}
		if (obj instanceof ErlangFunction) {
			final ErlangFunction f = (ErlangFunction) obj;
			return f.arity == arity && f.name.equals(name);
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

}
