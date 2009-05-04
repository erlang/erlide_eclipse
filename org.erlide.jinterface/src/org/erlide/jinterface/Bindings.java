/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.jinterface;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.erlide.jinterface.rpc.RpcConverter;
import org.erlide.jinterface.rpc.RpcException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class Bindings {

	private final Map<String, OtpErlangObject> bindings;

	public Bindings() {
		this.bindings = new HashMap<String, OtpErlangObject>();
	}

	public Bindings(Bindings other) {
		this();
		merge(other);
	}

	public void merge(Bindings other) {
		this.bindings.putAll(other.bindings);
	}

	public OtpErlangObject get(String name) {
		return this.bindings.get(name);
	}

	public long getLong(String name) throws OtpErlangException {
		OtpErlangObject r = get(name);
		if (r instanceof OtpErlangLong) {
			return ((OtpErlangLong) r).longValue();
		}
		throw new OtpErlangException("value is not an integer");
	}

	public String getAtom(String name) throws OtpErlangException {
		OtpErlangObject r = get(name);
		if (r instanceof OtpErlangAtom) {
			return ((OtpErlangAtom) r).atomValue();
		}
		throw new OtpErlangException("value is not an atom");
	}

	public String getString(String name) throws OtpErlangException {
		OtpErlangObject r = get(name);
		if (r instanceof OtpErlangString) {
			return ((OtpErlangString) r).stringValue();
		}
		throw new OtpErlangException("value is not a string");
	}

	public OtpErlangObject[] getList(String name) throws OtpErlangException {
		OtpErlangObject r = get(name);
		if (r instanceof OtpErlangList) {
			return ((OtpErlangList) r).elements();
		}
		throw new OtpErlangException("value is not a list");
	}

	public OtpErlangObject[] getTuple(String name) throws OtpErlangException {
		OtpErlangObject r = get(name);
		if (r instanceof OtpErlangTuple) {
			return ((OtpErlangTuple) r).elements();
		}
		throw new OtpErlangException("value is not a tuple");
	}

	@SuppressWarnings("unchecked")
	public <T> T getAs(String name, Class<T> cls) throws RpcException {
		OtpErlangObject v = get(name);
		return (T) RpcConverter.erlang2java(v, cls);
	}

	public void put(String name, OtpErlangObject value) {
		this.bindings.put(name, value);
	}

	public Map<String, OtpErlangObject> getAll() {
		return Collections.unmodifiableMap(this.bindings);
	}

	@Override
	public String toString() {
		return bindings.toString();
	}

}
