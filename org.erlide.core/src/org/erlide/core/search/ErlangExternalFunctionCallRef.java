package org.erlide.core.search;

import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.core.erlang.internal.SourceRange;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlangExternalFunctionCallRef extends ErlangElementRef {
	private final String module;
	private final String function;
	private final int arity;
	private ISourceRange pos;
	private IErlElement parent;

	public ErlangExternalFunctionCallRef(final String module,
			final String function, final int arity) {
		super();
		this.module = module;
		this.function = function;
		this.arity = arity;
		pos = null;
		parent = null;
	}

	public ErlangExternalFunctionCallRef(final OtpErlangTuple t) {
		final OtpErlangAtom m = (OtpErlangAtom) t.elementAt(1);
		final OtpErlangAtom f = (OtpErlangAtom) t.elementAt(2);
		final OtpErlangLong a = (OtpErlangLong) t.elementAt(3);
		final OtpErlangLong p = (OtpErlangLong) t.elementAt(4);
		final OtpErlangLong l = (OtpErlangLong) t.elementAt(5);
		module = m.atomValue();
		function = f.atomValue();
		int ar;
		try {
			ar = a.intValue();
		} catch (final OtpErlangRangeException e) {
			ar = -1;
		}
		arity = ar;
		try {
			setPos(p.intValue(), l.intValue());
		} catch (final OtpErlangRangeException e) {
		}
	}

	/**
	 * @return the module
	 */
	public String getModule() {
		return module;
	}

	/**
	 * @return the function
	 */
	public String getFunction() {
		return function;
	}

	/**
	 * @return the arity
	 */
	public int getArity() {
		return arity;
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		String s = "";
		if (pos != null) {
			s = " (" + pos.getOffset() + " . " + pos.getLength() + ")";
		}
		if (parent != null) {
			s += " in " + parent.toString();
		}
		return getModule() + ":" + getFunction() + "/" + getArity() + s;
	}

	public void setPos(final ISourceRange pos) {
		this.pos = pos;
	}

	public void setPos(final int start, final int len) {
		pos = new SourceRange(start, len);
	}

	public ISourceRange getPos() {
		return pos;
	}

	/**
	 * @return the parent
	 */
	public IErlElement getParent() {
		return parent;
	}

	/**
	 * @param parent
	 *            the parent to set
	 */
	public void setParent(final IErlElement parent) {
		this.parent = parent;
	}

}
