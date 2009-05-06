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
package org.erlide.jinterface.bridge;

import java.io.IOException;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.Map;

import org.erlide.jinterface.rpc.RpcConverter;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

public class ErlangBridge {

	@SuppressWarnings( { "boxing", "unchecked" })
	public static void main(final String[] args) {
		final Map<String, Integer> o = (Map<String, Integer>) newInstance(Map.class,
				"wolf", null);
		o.put("dd", 44);

	}

	/**
	 * Given an interface, an erlang node name and a module name, construct an
	 * object implementing that interface that forwards the calls via RPC to the
	 * node, to the named module. The function names are the same as the
	 * method's.
	 * 
	 * @param intf
	 * @param node
	 * @param module
	 * @return
	 */
	public static Object newInstance(final Class<?> intf, final String node, final String module) {
		try {
			return Proxy.newProxyInstance(intf.getClassLoader(),
					new Class[] { intf }, new ErlangBridgeHandler(intf
							.getCanonicalName(), node, module));
		} catch (final Exception e) {
			e.printStackTrace();
			return null;
		}

	}

	public static Object newInstance(final Class<?> intf, final String node) {
		return newInstance(intf, node, intf.getName().replaceAll("\\.", "_"));
	}

	public static class ErlangBridgeHandler implements InvocationHandler {

		@SuppressWarnings("unused")
		private String module;
		@SuppressWarnings("unused")
		private final OtpMbox mbox;
		private OtpNode lnode;

		public ErlangBridgeHandler(final String intf, final String node, final String module) {
			this.module = module;
			if (module == null) {
				this.module = intf;
			}
			try {
				this.lnode = new OtpNode("dummy");
			} catch (final IOException e) {
				e.printStackTrace();
			}
			// TODO where is the mbox closed?
			this.mbox = this.lnode.createMbox();
		}

		public Object invoke(final Object proxy, final Method method, final Object[] args)
		throws Throwable {
			final OtpErlangObject[] eargs = new OtpErlangObject[args.length + 1];
			eargs[0] = RpcConverter.java2erlang(proxy, "x");
			for (int i = 0; i < args.length; i++) {
				eargs[i + 1] = RpcConverter.java2erlang(args[i], "x");
			}
			// OtpErlangObject msg = RpcUtil.buildRpcCall(this.mbox.self(),
			// this.module, method.getName(), eargs);
			//
			// System.out.println("-->" + msg);
			// mbox.send("rex", node, msg);
			// OtpErlangTuple res = (OtpErlangTuple) this.mbox.receive(5000);
			// if (res == null) {
			return null;
			// }
			// return res.elementAt(1);
		}

	}

}
