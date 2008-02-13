package org.erlide.jinterface.bridge;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

public class ErlangInvocationHandler implements InvocationHandler {

	public ErlangInvocationHandler(Class<?> c, Object args) {
	}

	public Object invoke(Object proxy, Method method, Object[] args)
			throws Throwable {

		System.out.format(">> wrap: %s.%s(%s)", proxy.toString(), method
				.getName(), args);

		return null;
	}

}
