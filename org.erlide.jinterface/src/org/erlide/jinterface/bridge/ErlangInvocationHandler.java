package org.erlide.jinterface.bridge;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.Arrays;

public class ErlangInvocationHandler implements InvocationHandler {

	public ErlangInvocationHandler(final Class<?> c, final Object args) {
	}

	public Object invoke(final Object proxy, final Method method, final Object[] args)
	throws Throwable {

		System.out.format(">> wrap: %s.%s(%s)", proxy.toString(), method
				.getName(), Arrays.toString(args));

		return null;
	}

}
