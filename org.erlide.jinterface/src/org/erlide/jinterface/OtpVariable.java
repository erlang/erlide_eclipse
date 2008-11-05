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

import org.erlide.jinterface.rpc.Signature;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpOutputStream;

/**
 * Provides a Java representation of Erlang variables. !!! These are to NOT to
 * be sent to an Erlang node !!!! Their use is in pattern matching only.
 */
public class OtpVariable extends OtpErlangObject {

	private static final long serialVersionUID = -1L;

	private String name;
	private Signature sign;

	public OtpVariable(String n) {
		String[] v = n.split(":");
		name = v[0];
		if (v.length > 1) {
			sign = new Signature(v[1].charAt(0));
		} else {
			sign = new Signature('x');
		}
	}

	public String getName() {
		return name;
	}

	public Signature getSignature() {
		return sign;
	}

	@Override
	public String toString() {
		return "'%" + name + ":" + sign.kind + "'";
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof OtpVariable)) {
			return false;
		}

		final OtpVariable l = (OtpVariable) o;
		return name.equals(l.name) && sign.equals(l.sign);
	}

	@Override
	public int hashCode() {
		return name.hashCode() + sign.hashCode() * 31;
	}

	@Override
	public void encode(OtpOutputStream arg0) {
		// throw new NotImplementedException();
	}

}