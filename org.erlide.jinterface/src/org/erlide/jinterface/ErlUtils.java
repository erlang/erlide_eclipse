/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.jinterface;

import java.util.ArrayList;
import java.util.List;

import org.erlide.jinterface.rpc.RpcConverter;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.jinterface.rpc.Signature;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlUtils {

	public static OtpErlangObject parse(String string) throws ParserException {
		return TermParser.parse(string);
	}

	/**
	 * for example, <code> format("{hello, ~s, [~a, _]}", "myname", "mykey")
	 * </code> gives the equivalent of
	 * <code> {hello, "myname", [mykey, _]}
	 * </code>.
	 * 
	 * @throws RpcException
	 */
	public static OtpErlangObject format(String fmt, String sign,
			Object... args) throws ParserException, RpcException {
		OtpErlangObject result;
		result = parse(fmt);
		List<OtpErlangObject> values = new ArrayList<OtpErlangObject>(
				args.length);
		Signature[] signs = RpcConverter.parseSignature(sign);
		if (signs.length != args.length) {
			throw new ParserException("signature doesn't match value list");
		}
		for (int i = 0; i < signs.length; i++) {
			values.add(RpcConverter.java2erlang(args[i], signs[i]));
		}
		result = fill(result, values);
		return result;
	}

	public static Bindings match(String pattern, String term)
			throws ParserException {
		return match(parse(pattern), parse(term), new Bindings());
	}

	public static Bindings match(String pattern, OtpErlangObject term)
			throws ParserException {
		return match(parse(pattern), term, new Bindings());
	}

	public static Bindings match(String pattern, OtpErlangObject term,
			Bindings bindings) throws ParserException {
		return match(parse(pattern), term, bindings);
	}

	public static Bindings match(String pattern, String term, Bindings bindings)
			throws ParserException {
		return match(parse(pattern), parse(term), bindings);
	}

	public static Bindings match(OtpErlangObject pattern, OtpErlangObject term) {
		return match(pattern, term, new Bindings());
	}

	public static Bindings match(OtpErlangObject pattern, String term)
			throws ParserException {
		return match(pattern, parse(term), new Bindings());
	}

	public static Bindings match(OtpErlangObject pattern, OtpErlangObject term,
			Bindings bindings) {
		if (pattern instanceof OtpVariable) {
			OtpVariable var = (OtpVariable) pattern;
			if (!RpcConverter.matchSignature(term, var.getSignature())) {
				return null;
			}
			if (var.getName().equals("_")) {
				return bindings;
			}
			final Bindings result = new Bindings(bindings);
			final OtpErlangObject old = bindings.get(var.getName());
			// no previous binding
			if (old == null) {
				result.put(var.getName(), term);
				return result;
			}
			return old.equals(term) ? result : null;
		}
		if (!pattern.getClass().equals(term.getClass())) {
			return null;
		}

		if (pattern.equals(term)) {
			return bindings;
		} else if (pattern instanceof OtpErlangList) {
			return matchList(((OtpErlangList) pattern).elements(),
					((OtpErlangList) term).elements(), bindings);
		} else if (pattern instanceof OtpErlangTuple) {
			return matchList(((OtpErlangTuple) pattern).elements(),
					((OtpErlangTuple) term).elements(), bindings);
		}
		return null;
	}

	private static OtpErlangObject fill(OtpErlangObject template,
			List<OtpErlangObject> values) {
		if (values.size() == 0) {
			return template;
		}
		if (template instanceof OtpErlangList) {
			OtpErlangObject[] elements = ((OtpErlangList) template).elements();
			List<OtpErlangObject> result = new ArrayList<OtpErlangObject>(
					elements.length);
			for (OtpErlangObject elem : elements) {
				result.add(fill(elem, values));
			}
			return new OtpErlangList(result.toArray(elements));
		} else if (template instanceof OtpErlangTuple) {
			OtpErlangObject[] elements = ((OtpErlangTuple) template).elements();
			List<OtpErlangObject> result = new ArrayList<OtpErlangObject>(
					elements.length);
			for (OtpErlangObject elem : elements) {
				result.add(fill(elem, values));
			}
			return new OtpErlangTuple(result.toArray(elements));
		} else if (template instanceof OtpPlaceholder) {
			return values.remove(0);
		} else {
			return template;
		}
	}

	private static Bindings matchList(OtpErlangObject[] patterns,
			OtpErlangObject[] terms, Bindings bindings) {
		// System.out.println("match list");
		Bindings result = new Bindings(bindings);
		for (int i = 0; i < patterns.length; i++) {
			result = match(patterns[i], terms[i], result);
			if (result == null) {
				return null;
			}
		}
		return result;
	}

}