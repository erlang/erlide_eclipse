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

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpOutputStream;

/**
 * Provides a Java representation of Erlang format placeholders. !!! These are
 * to NOT to be sent to an Erlang node !!!! Their use is in formatting only.
 */
public class OtpPlaceholder extends OtpErlangObject {

	private static final long serialVersionUID = -1L;

	private String name;

	public OtpPlaceholder(String n) {
		this.name = n;
	}

	public String getName() {
		return this.name;
	}

	@Override
	public String toString() {
		return this.name;
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof OtpPlaceholder)) {
			return false;
		}

		final OtpPlaceholder l = (OtpPlaceholder) o;
		return this.name.equals(l.name);
	}

	@Override
	public int hashCode() {
		return this.name.hashCode();
	}

	@Override
	public void encode(OtpOutputStream arg0) {
		// throw new NotImplementedException();
	}

}