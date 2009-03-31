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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.erlide.core.erlang.util.Util;
import org.erlide.jinterface.rpc.RpcConverter;
import org.erlide.jinterface.rpc.generator.RpcStubGenerator;

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
		if (Util.isOk(v)) {
			return v.elementAt(1);
		}
		return v;
	}

	public static void generateRpcStub(final String className,
			final boolean onlyDeclared, final Backend b) {
		generateRpcStub(RpcConverter.getClassByName(className), onlyDeclared, b);
	}

	public static void generateRpcStub(final Class<?> cls,
			final boolean onlyDeclared, final Backend b) {
		final String s = RpcStubGenerator.generate(cls, onlyDeclared);
		ErlideBackend.generateRpcStub(b, s);
	}

	public static IProject[] getProjects(final String attribute) {
		final String[] projectNames = attribute.split(";");
		return getProjects(projectNames);
	}

	public static IProject[] getProjects(final String[] projectNames) {
		final List<IProject> projects = new ArrayList<IProject>();
		final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		for (final String s : projectNames) {
			if (s != null && s.length() > 0) {
				final IProject p = root.getProject(s);
				if (p != null) {
					projects.add(p);
				}
			}
		}
		return projects.toArray(new IProject[projects.size()]);
	}
}
