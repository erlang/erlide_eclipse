/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package com.ericsson.otp.erlang;

import org.erlide.jinterface.rpc.Signature;

/**
 * Provides a Java representation of Erlang variables.
 * <p>
 * <b>!!! These are to NOT to be sent to an Erlang node !!!!</b> Their use is in
 * pattern matching only.
 */
public class OtpPatternVariable extends OtpErlangObject {

	private static final long serialVersionUID = -1L;

	private final String name;
	private Signature sign;

	public OtpPatternVariable(final String n) {
		final String[] v = n.split(":");
		this.name = v[0];
		if (v.length > 1) {
			this.sign = new Signature(v[1].charAt(0));
		} else {
			this.sign = new Signature('x');
		}
	}

	public String getName() {
		return this.name;
	}

	public Signature getSignature() {
		return this.sign;
	}

	@Override
	public String toString() {
		return "'%" + this.name + ":" + this.sign.kind + "'";
	}

	@Override
	public boolean equals(final Object o) {
		if (!(o instanceof OtpPatternVariable)) {
			return false;
		}

		final OtpPatternVariable l = (OtpPatternVariable) o;
		return this.name.equals(l.name) && this.sign.equals(l.sign);
	}

	@Override
	public int hashCode() {
		return this.name.hashCode() + this.sign.hashCode() * 31;
	}

	@Override
	public void encode(final OtpOutputStream arg0) {
		// throw new NotImplementedException();
	}

}
