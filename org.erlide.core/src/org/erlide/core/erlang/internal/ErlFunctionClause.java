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

public class ErlFunctionClause extends ErlMember implements IErlFunctionClause {

	String arguments;
	String guards;

	protected ErlFunctionClause(final IErlElement parent, final String name,
			final String arguments, final String guards) {
		super(parent, name);
		this.arguments = arguments;
		this.guards = guards;
	}

	public String getArguments() {
		return arguments;
	}

	public String getGuards() {
		return guards;
	}

	public Kind getKind() {
		return Kind.CLAUSE;
	}

	/**
	 * @param arguments
	 *            the arguments to set
	 */
	// public void setArguments(final String arguments) {
	// this.arguments = arguments;
	// }
	/**
	 * @param guards
	 *            the guards to set
	 */
	// public void setGuards(final OtpErlangList guards) {
	// this.guards = guards;
	// }
	@Override
	public String toString() {
		if (guards != null && guards != "") {
			return arguments + " when " + guards;
		} else {
			return arguments;
			// String result = pp_1(getGuards());
			// if (result.length() > 0) {
			// result = pp(getArguments()) + " when " + result;
			// } else {
			// result = pp(getArguments());
			// }
			// return result;
		}
	}
}
