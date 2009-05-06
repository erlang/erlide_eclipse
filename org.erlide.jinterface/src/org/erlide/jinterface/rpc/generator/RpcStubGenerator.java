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
package org.erlide.jinterface.rpc.generator;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.erlide.jinterface.rpc.RpcConverter;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangRef;

/**
 * Takes a fully qualified class name and generates an erlang stub module that
 * uses rpc to call the class and its instances.
 * 
 * Restrictions: - can't handle the difference between int and long
 * 
 */
public class RpcStubGenerator {

	public static String generate(final Class<?> clazz, final boolean onlyDeclared) {
		return generate(clazz.getName(), clazz.getClassLoader(), onlyDeclared);
	}

	public static String generate(final Class<?> clazz, final ClassLoader cl,
			final boolean onlyDeclared) {
		return generate(clazz.getName(), cl, onlyDeclared);
	}

	public static String generate(final String className, final ClassLoader cl,
			final boolean onlyDeclared) {
		try {
			final Class<?> clazz = Class.forName(className, true, cl);

			final String moduleName = module(clazz);

			return generate(clazz, moduleName, onlyDeclared);
		} catch (final ClassNotFoundException e) {
			e.printStackTrace();
			return e.getMessage();
		}
	}

	@SuppressWarnings("boxing")
	private static String generate(final Class<?> clazz, final String moduleName,
			final boolean onlyDeclared) {
		final StringBuilder buf = new StringBuilder();
		buf.append(String.format("-module(%s).%n-compile(export_all).%n%n",
				moduleName));

		// TODO add constructors
		final Constructor<?>[] constructors = onlyDeclared ? clazz
				.getDeclaredConstructors() : clazz.getConstructors();
				final Map<Integer, List<Constructor<?>>> cmap = new HashMap<Integer, List<Constructor<?>>>();
				for (final Constructor<?> constructor : constructors) {
					final int plen = constructor.getParameterTypes().length;
					List<Constructor<?>> list = cmap.get(plen);
					if (list == null) {
						list = new ArrayList<Constructor<?>>();
					}
					list.add(constructor);
					cmap.put(plen, list);
				}

				for (final Entry<Integer, List<Constructor<?>>> entry : cmap.entrySet()) {
					final List<Constructor<?>> list = entry.getValue();
					Collections.sort(list, new Comparator<Constructor<?>>() {

						public int compare(final Constructor<?> m1, final Constructor<?> m2) {
							final Class<?>[] p1 = m1.getParameterTypes();
							final Class<?>[] p2 = m2.getParameterTypes();
							for (int i = 0; i < p1.length; i++) {
								final Class<?> t1 = RpcConverter.javaType2erlang(p1[i]);
								final Class<?> t2 = RpcConverter.javaType2erlang(p2[i]);

								int result = -2;
								if (t1 == OtpErlangRef.class) {
									result = 1;
								}
								if (t2 == OtpErlangRef.class) {
									result = -1;
								}
								if (result != -2) {
									return result;
								}
							}
							return 0;
						}

					});
					for (final Constructor<?> constructor : list) {
						printClause(clazz, buf, constructor);
						if (list.indexOf(constructor) == list.size() - 1) {
							buf.append(".\n\n");
						} else {
							buf.append(";\n");
						}
					}
				}

				final Method[] methods = onlyDeclared ? clazz.getDeclaredMethods() : clazz
						.getMethods();

				final Map<Tuple, List<Method>> mmap = new HashMap<Tuple, List<Method>>();
				for (final Method method : methods) {
					final int mod = method.getModifiers();
					final boolean statik = Modifier.isStatic(mod);
					final int plen = method.getParameterTypes().length;
					final Tuple key = new Tuple().add(method.getName()).add(
							statik ? plen : plen + 1);
					List<Method> list = mmap.get(key);
					if (list == null) {
						list = new ArrayList<Method>();
					}
					list.add(method);
					mmap.put(key, list);
				}

				for (final Entry<Tuple, List<Method>> entry : mmap.entrySet()) {
					final List<Method> list = entry.getValue();
					Collections.sort(list, new Comparator<Method>() {

						public int compare(final Method m1, final Method m2) {
							final Class<?>[] p1 = getExParams(m1);
							final Class<?>[] p2 = getExParams(m2);
							for (int i = 0; i < p1.length; i++) {
								final Class<?> t1 = RpcConverter.javaType2erlang(p1[i]);
								final Class<?> t2 = RpcConverter.javaType2erlang(p2[i]);

								int result = -2;
								if (t1 == OtpErlangRef.class) {
									result = 1;
								}
								if (t2 == OtpErlangRef.class) {
									result = -1;
								}
								if (result != -2) {
									return result;
								}
							}
							return 0;
						}

					});
					for (final Method method : list) {
						printClause(clazz, buf, method);
						if (list.indexOf(method) == list.size() - 1) {
							buf.append(".\n\n");
						} else {
							buf.append(";\n");
						}
					}
				}

				return buf.toString();

	}

	private static void printClause(final Class<?> clazz, final StringBuilder buf,
			final Constructor<?> constructor) {
		final Class<?>[] params = constructor.getParameterTypes();

		buf.append("  'new'(");

		printParams(true, buf, params);
		StringBuilder guards = new StringBuilder();
		final Class<?>[] p = constructor.getParameterTypes();
		for (int i = 0; i < p.length; i++) {
			final String name = "P" + i;
			final String grd = mkGuard(RpcConverter.javaType2erlang(p[i]), name);
			guards.append(grd);
			if ((i < p.length - 1) && grd.length() > 0) {
				guards.append(", ");
			}
		}
		if (guards.length() != 0) {
			guards = new StringBuilder("when ").append(guards);
		}
		buf.append(") " + guards + " ->\n");

		buf.append("    erlide_jrpc:call(");
		buf.append("<<\"" + clazz.getName() + "\">>, ");

		final StringBuilder args = new StringBuilder();
		final Class<?>[] at = constructor.getParameterTypes();
		for (int i = 0; i < at.length; i++) {
			args.append("<<\"" + at[i].getName() + "\">>");
			if (i < at.length - 1) {
				args.append(", ");
			}
		}
		buf.append("{<<\"" + constructor.getName() + "\">>, [").append(args)
		.append("]}, [");
		printParams(true, buf, params);
		buf.append("])");
	}

	static Class<?>[] getExParams(final Method m1) {
		Class<?>[] p1 = m1.getParameterTypes();
		final int mod = m1.getModifiers();
		final boolean statik = Modifier.isStatic(mod);
		if (!statik) {
			p1 = new Class<?>[p1.length + 1];
			p1[0] = m1.getDeclaringClass();
			System.arraycopy(m1.getParameterTypes(), 0, p1, 1, p1.length - 1);
		}
		return p1;
	}

	private static void printClause(final Class<?> clazz, final StringBuilder buf,
			final Method method) {
		final int mod = method.getModifiers();
		final boolean statik = Modifier.isStatic(mod);
		final Class<?>[] params = method.getParameterTypes();

		buf.append("  '" + method.getName() + "'(");
		if (!statik) {
			buf.append("Obj");
		}

		printParams(statik, buf, params);
		final Class<?>[] p = getExParams(method);
		boolean supported = true;
		for (int i = 0; i < p.length; i++) {
			if (p[i].isArray() && p[i].getComponentType().isPrimitive()) {
				supported = false;
			}
		}
		if (statik) {
			buf.append(") ->\n");
		} else {
			buf.append(") when is_reference(Obj) ->\n");
		}

		if (supported) {
			if (method.getReturnType() == Void.TYPE) {
				buf.append("    erlide_jrpc:cast(");
			} else {
				buf.append("    %% ");
				buf.append(" returns " + method.getReturnType().getName()
						+ "\n");
				buf.append("    erlide_jrpc:call(");
			}
			if (statik) {
				buf.append("<<\"" + clazz.getName() + "\">>, ");
			} else {
				buf.append("Obj, ");
			}
			final StringBuilder args = new StringBuilder();
			final Class<?>[] at = method.getParameterTypes();
			for (int i = 0; i < at.length; i++) {
				args.append("<<\"" + at[i].getName() + "\">>");
				if (i < at.length - 1) {
					args.append(", ");
				}
			}
			buf.append("{<<\"" + method.getName() + "\">>, [").append(args)
			.append("]}, [");
			printParams(true, buf, params);
			buf.append("])");
		} else {
			buf.append("    {error, not_supported}");
		}
	}

	private static String mkGuard(final Class<?> param, final String name) {
		if (param == OtpErlangLong.class) {
			return "is_integer(" + name + ")";
		}
		if (param == OtpErlangAtom.class) {
			return "is_atom(" + name + ")";
		}
		if (param == OtpErlangDouble.class) {
			return "is_float(" + name + ")";
		}
		if (param == OtpErlangList.class) {
			return "is_list(" + name + ")";
		}
		// if (param == OtpErlangString.class) {
		// return "is_list(" + name + ")";
		// }
		// if (param == OtpErlangTuple.class) {
		// return "is_tuple(" + name + ")";
		// }
		// if (param == OtpErlangRef.class) {
		// return "";
		// }
		return "";
	}

	private static void printParams(final boolean nohdr, final StringBuilder buf,
			final Class<?>[] params) {
		if (!nohdr && params.length > 0) {
			buf.append(", ");
		}
		if (params.length > 0) {
			buf.append("P0");
		}
		for (int i = 1; i < params.length; i++) {
			buf.append(", " + "P" + i);
		}
	}

	public static String module(final Class<?> clazz) {
		return clazz.getName().replaceAll("\\.", "_");
	}

}
