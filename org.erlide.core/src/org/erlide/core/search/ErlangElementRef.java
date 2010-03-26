package org.erlide.core.search;

import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlElement.Kind;
import org.erlide.core.erlang.util.ErlangFunction;

import com.ericsson.otp.erlang.OtpErlangObject;

public abstract class ErlangElementRef {

	public abstract OtpErlangObject getSearchObject();

	public abstract String searchElementToString(ErlangSearchElement ese);

	abstract public Kind searchElementToKind(ErlangSearchElement ese);

	public static IErlElement.Kind searchElementFunctionToKind(
			final ErlangSearchElement ese) {
		if (ese.isSubClause()) {
			return Kind.CLAUSE;
		} else {
			return Kind.FUNCTION;
		}
	}

	public static String searchElementFunctionToString(
			final ErlangSearchElement ese) {
		ErlangFunction f = ese.getFunction();
		String a = ese.getArguments();
		if (ese.isSubClause()) {
			return f.name + a;
		} else {
			final String nameWithArity = f.getNameWithArity();
			if (a != null) {
				return nameWithArity + "  " + a;
			} else {
				return nameWithArity;
			}
		}
	}

}
