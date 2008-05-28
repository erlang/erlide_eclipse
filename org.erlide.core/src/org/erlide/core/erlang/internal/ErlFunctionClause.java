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

	String head;

	protected ErlFunctionClause(final IErlElement parent, final String name,
			final String head) {
		super(parent, name);
		this.head = head;
	}

	public String getHead() {
		return head;
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
		return head;
	}
}
