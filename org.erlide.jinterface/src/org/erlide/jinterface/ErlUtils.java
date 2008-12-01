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
import java.util.Arrays;
import java.util.List;

import org.erlide.jinterface.rpc.RpcConverter;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.jinterface.rpc.Signature;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlUtils {

	private ErlUtils() {
	}

	public static OtpErlangObject parse(String string) throws ParserException {
		return TermParser.parse(string);
	}

	/**
	 * Build an Erlang (extended) term from a textual description. For example,
	 * <code> format("{hello, ~s, [~a, _]}", "myname", "mykey")
	 * </code> gives the equivalent of <code> {hello, "myname", [mykey, _]}
	 * </code>.
	 * <p>
	 * Items beginning with ~ are placeholders that will be replaced with the
	 * corresponding argument (from left to right). The text after the ~ is the
	 * type signature of the argument, so that automatic conversion Java->Erlang
	 * can be done. See RpcConverter.java2erlang for details.
	 * 
	 * @throws RpcException
	 * @see org.erlide.jinterface.rpc.RpcConverter
	 */
	public static OtpErlangObject format(String fmt, Object... args)
			throws ParserException, RpcException {
		OtpErlangObject result;
		result = parse(fmt);
		List<Object> values = new ArrayList<Object>(args.length);
		values = new ArrayList<Object>(Arrays.asList(args));
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

	/**
	 * Match two Erlang terms.
	 * <p>
	 * Variables can have a type signature attached, like for example
	 * <code>Var:i</code>. Its meaning is that the type of the value must match
	 * too.
	 * <p>
	 * The returned value is null if there was any mismatch, otherwise it is a
	 * map of variable names to matched values. <br>
	 * TODO should we throw an exception instead?
	 * 
	 * @throws RpcException
	 */
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
			List<Object> values) throws RpcException, ParserException {
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
			OtpPlaceholder holder = (OtpPlaceholder) template;
			Object ret = values.remove(0);
			Signature[] signs = Signature.parse(holder.getName());
			if (signs.length == 0 && !(ret instanceof OtpErlangObject)) {
				throw new ParserException("funny placeholder");
			}
			Signature sign = (signs.length == 0) ? new Signature('x')
					: signs[0];
			return RpcConverter.java2erlang(ret, sign);
		} else {
			return template;
		}
	}

	private static Bindings matchList(OtpErlangObject[] patterns,
			OtpErlangObject[] terms, Bindings bindings) {
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