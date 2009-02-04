/*******************************************************************************
 * Copyright (c) 2009 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package com.ericsson.otp.erlang;

/**
 * Provides a Java representation of ellipsis in list and tuple representation.
 * A list's or tuple's last element can be an ellipsis, meaning that the rest of
 * the elements are ignored in the pattern matching.
 * <p>
 * <b>!!! These are to NOT to be sent to an Erlang node !!!!</b> Their use is in
 * formatting only.
 */
public class OtpEllipsis extends OtpErlangObject {

	private static final long serialVersionUID = -1L;

	public OtpEllipsis() {
	}

	@Override
	public String toString() {
		return "...";
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof OtpEllipsis)) {
			return false;
		}
		return true;
	}

	@Override
	public int hashCode() {
		return "...".hashCode();
	}

	@Override
	public void encode(OtpOutputStream arg0) {
		// throw new NotImplementedException();
	}

}
