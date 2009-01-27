package org.erlide.jinterface.bridge;

import java.lang.reflect.Proxy;

/**
 * 
 * 
 * @author vladdu
 * 
 */
public class ErlangWrapperFactory {
	public static Object wrap(Class<?> c, Object args) {
		return Proxy.newProxyInstance(c.getClassLoader(), new Class[] { c },
				new ErlangInvocationHandler(c, args));
	}
}
