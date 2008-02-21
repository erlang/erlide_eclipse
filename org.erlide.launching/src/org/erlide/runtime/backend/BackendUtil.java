/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.backend;

import java.io.IOException;
import java.net.URL;

import org.eclipse.core.runtime.FileLocator;
import org.erlide.jinterface.rpc.RpcConverter;
import org.erlide.jinterface.rpc.generator.RpcStubGenerator;
import org.erlide.runtime.ErlangLaunchPlugin;
import org.erlide.runtime.backend.exceptions.ErlangEvalException;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideBackend;

/**
 * 
 * 
 * @author Vlad Dumitrescu [vladdu55 at gmail dot com]
 */
public class BackendUtil {

	public static OtpErlangObject ok(OtpErlangObject v0) {
		if (!(v0 instanceof OtpErlangTuple)) {
			return v0;
		}

		final OtpErlangTuple v = (OtpErlangTuple) v0;
		if (((OtpErlangAtom) v.elementAt(0)).atomValue().compareTo("ok") == 0) {
			return v.elementAt(1);
		}
		return v;
	}

	/**
	 * 
	 * @param string
	 * @return OtpErlangObject
	 * @throws ErlangEvalException
	 */
	public static BackendEvalResult eval(IBackend b, String string) {
		return ErlideBackend.eval(b, string, null);
	}

	public static String getLocalPath(String string) {
		String dir = "?";
		final Bundle b = ErlangLaunchPlugin.getDefault().getBundle();
		final URL url = b.getEntry(string);
		try {
			dir = FileLocator.toFileURL(url).getPath().substring(1);
		} catch (final IOException e) {
			dir = "!?";
			e.printStackTrace();
		}
		return dir;
	}

	public static void generateRpcStub(String className, boolean onlyDeclared,
			IBackend b) {
		generateRpcStub(RpcConverter.getClassByName(className), onlyDeclared, b);
	}

	public static void generateRpcStub(Class<?> cls, boolean onlyDeclared,
			IBackend b) {
		String s = RpcStubGenerator.generate(cls, onlyDeclared);
		ErlideBackend.generateRpcStub(b, s);
	}

}
