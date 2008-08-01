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
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
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

	public static OtpErlangObject ok(final OtpErlangObject v0) {
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
	public static BackendEvalResult eval(final ExecutionBackend b,
			final String string) {
		return ErlideBackend.eval(b, string, null);
	}

	public static String getLocalPath(final String string) {
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

	public static void generateRpcStub(final String className,
			final boolean onlyDeclared, final BuildBackend b) {
		generateRpcStub(RpcConverter.getClassByName(className), onlyDeclared, b);
	}

	public static void generateRpcStub(final Class<?> cls,
			final boolean onlyDeclared, final BuildBackend b) {
		final String s = RpcStubGenerator.generate(cls, onlyDeclared);
		ErlideBackend.generateRpcStub(b, s);
	}

	public static IProject[] getProjects(final String attribute) {
		final String[] otherProjectNames = attribute.split(";");
		final List<IProject> otherProjects = new ArrayList<IProject>();
		final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		for (final String s : otherProjectNames) {
			if (s != null && s.length() > 0) {
				final IProject p = root.getProject(s);
				if (p != null) {
					otherProjects.add(p);
				}
			}
		}
		return otherProjects.toArray(new IProject[otherProjects.size()]);
	}
}
