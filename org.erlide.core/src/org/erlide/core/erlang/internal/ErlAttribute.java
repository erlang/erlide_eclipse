/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.erlang.internal;

import org.erlide.core.erlang.IErlAttribute;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * 
 * 
 * @author Vlad Dumitrescu
 */
public class ErlAttribute extends ErlMember implements IErlAttribute {

	private final OtpErlangObject fValue;

	/**
	 * @param parent
	 * @param name
	 */
	protected ErlAttribute(ErlElement parent, String name, OtpErlangObject value) {
		super(parent, name);
		fValue = value;
	}

	/**
	 * @see org.erlide.core.erlang.IErlElement#getKind()
	 */
	public Kind getKind() {
		return Kind.ATTRIBUTE;
	}

	public OtpErlangObject getValue() {
		return fValue;
	}

	@Override
	public OtpErlangObject getParseTree() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String toString() {
		String sval;
		if (fValue != null) {
			sval = ": " + pp(fValue);
		} else {
			sval = "";
		}
		return getName() + sval;
	}
}
