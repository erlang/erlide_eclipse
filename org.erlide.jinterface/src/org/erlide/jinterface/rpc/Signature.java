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

public class Signature {
	public char kind = 'x';
	public Signature[] content = null;

	public Signature(final char str) {
		kind = str;
	}

	public Signature(final char crt, final Signature sub) {
		kind = crt;
		content = new Signature[] { sub };
	}

	public Signature(final char crt, final Signature[] sub) {
		kind = crt;
		content = sub;
	}

	@Override
	public String toString() {
		String res = "";
		if (content != null) {
			res = "(";
			for (final Signature s : content) {
				res += s.toString() + ",";
			}
			res = res.substring(0, res.length() - 1) + ")";
		}
		return kind + res;
	}
}
