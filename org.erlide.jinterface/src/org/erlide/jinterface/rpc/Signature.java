/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.jinterface.rpc;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Signature {
	private static final Map<String, Signature[]> cache = new HashMap<String, Signature[]>();

	public char kind = 'x';
	public Signature[] content = null;

	public Signature(final char str) {
		this.kind = str;
	}

	public Signature(final char crt, final Signature sub) {
		this.kind = crt;
		this.content = new Signature[] { sub };
	}

	public Signature(final char crt, final Signature[] sub) {
		this.kind = crt;
		this.content = sub;
	}

	@Override
	public String toString() {
		String res = "";
		if (this.content != null) {
			res = "(";
			for (final Signature s : this.content) {
				res += s.toString() + ",";
			}
			res = res.substring(0, res.length() - 1) + ")";
		}
		return this.kind + res;
	}

	public static synchronized Signature[] parse(String signature)
			throws RpcException {
		if (signature == null) {
			return null;
			// throw new RpcException("Signature is null");
		}
		Signature[] result = cache.get(signature);
		if (result != null) {
			return result;
		}
		final List<Signature> type = new ArrayList<Signature>();
		while (signature.length() > 0) {
			final ParseState e = parseOne(signature);
			type.add(e.sign);
			signature = e.rest;
		}
		result = type.toArray(new Signature[type.size()]);
		cache.put(signature, result);
		return result;
	}

	private static class ParseState {
		public ParseState(final Signature signature, final String substring) {
			this.sign = signature;
			this.rest = substring;
		}

		Signature sign;
		String rest;
	}

	private static ParseState parseOne(final String signature)
			throws RpcException {
		final char crt = signature.charAt(0);
		if ("xidabrjfpso".indexOf(crt) >= 0) {
			return new ParseState(new Signature(crt), signature.substring(1));
		} else if (crt == 'l') {
			final ParseState sub = parseOne(signature.substring(1));
			return new ParseState(new Signature(crt, sub.sign), sub.rest);
		} else if ("0123456789".indexOf(crt) >= 0) {
			final int n = Integer.parseInt(signature.substring(0, 1));
			final Signature[] sub = new Signature[n];
			String s = signature.substring(1);
			for (int i = 0; i < n; i++) {
				final ParseState state = parseOne(s);
				sub[i] = state.sign;
				s = state.rest;
			}
			return new ParseState(new Signature('t', sub), s);
		} else {
			throw new RpcException("unknown signature code: " + crt);
		}
	}

}
