/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
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
import org.erlide.jinterface.rpc.RpcUtil;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

public class ErlangBridge {

	@SuppressWarnings( { "boxing", "unchecked" })
	public static void main(String[] args) {
		Map<String, Integer> o = (Map<String, Integer>) newInstance(Map.class,
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
	public static Object newInstance(Class<?> intf, String node, String module) {
		try {
			return Proxy.newProxyInstance(intf.getClassLoader(),
					new Class[] { intf }, new ErlangBridgeHandler(intf
							.getCanonicalName(), node, module));
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}

	}

	public static Object newInstance(Class<?> intf, String node) {
		return newInstance(intf, node, intf.getName().replaceAll("\\.", "_"));
	}

	public static class ErlangBridgeHandler implements InvocationHandler {

		private String module;
		private OtpNode lnode;
		private final OtpMbox mbox;

		public ErlangBridgeHandler(String intf, String node, String module) {
			this.module = module;
			if (module == null) {
				this.module = intf;
			}
			try {
				lnode = new OtpNode("dummy");
			} catch (IOException e) {
				e.printStackTrace();
			}
			mbox = lnode.createMbox();
		}

		public Object invoke(Object proxy, Method method, Object[] args)
				throws Throwable {
			OtpErlangObject[] eargs = new OtpErlangObject[args.length + 1];
			eargs[0] = RpcConverter.java2erlang(proxy, "x");
			for (int i = 0; i < args.length; i++) {
				eargs[i + 1] = RpcConverter.java2erlang(args[i], "x");
			}
			OtpErlangObject msg = RpcUtil.buildRpcCall(mbox.self(),
					module, method.getName(), eargs);

			System.out.println("-->" + msg);

			// mbox.send("rex", node, msg);
			OtpErlangTuple res = (OtpErlangTuple) mbox.receive(5000);
			if (res == null) {
				return null;
			}
			return res.elementAt(1);
		}

	}

}
