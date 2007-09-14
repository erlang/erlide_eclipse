/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.erlang.internal;

import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlError;
import org.erlide.core.erlang.ISourceRange;

import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlError extends ErlMember implements IErlError {

	private String message;

	protected ErlError(IErlElement parent, String name)
			throws IllegalArgumentException {
		super(parent, name);
		message = name;
	}

	public ErlElementType getElementType() {
		return ErlElementType.ERROR;
	}

	public String getMessage() {
		return message;
	}

	public String getData() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public OtpErlangObject getParseTree() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public ISourceRange getNameRange() {
		return null;
	}

	@Override
	public String getHoverHelp() {
		return super.getHoverHelp();
	}

	@Override
	public String toString() {
		return "ERR: " + getMessage();
	}
}
