/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.erlang.internal;

import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunctionClause;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlFunctionClause extends ErlMember implements IErlFunctionClause {

	protected ErlFunctionClause(IErlElement parent, String name) {
		super(parent, name);
	}

	public OtpErlangList getArguments() {
		final OtpErlangTuple tree = (OtpErlangTuple) getParseTree();
		if (tree == null) {
			return null;
		}
		return (OtpErlangList) tree.elementAt(3);
	}

	public OtpErlangObject getBody() {
		final OtpErlangTuple tree = (OtpErlangTuple) getParseTree();
		if (tree == null) {
			return null;
		}
		return tree.elementAt(5);
	}

	public OtpErlangList getGuards() {
		final OtpErlangTuple tree = (OtpErlangTuple) getParseTree();
		if (tree == null) {
			return null;
		}
		return (OtpErlangList) tree.elementAt(4);
	}

	public ErlElementType getElementType() {
		return ErlElementType.CLAUSE;
	}

	@Override
	public String getHoverHelp() {
		return super.getHoverHelp();
	}

	@Override
	public String toString() {
		String result = pp_1(getGuards());
		if (result.length() > 0) {
			result = pp(getArguments()) + " when " + result;
		} else {
			result = pp(getArguments());
		}
		return result;
	}
}
