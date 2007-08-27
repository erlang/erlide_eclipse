package org.erlide.jinterface;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Takes a fully qualified class name and generates an erlang stub module that
 * uses rpc to call the class and its instances.
 * 
 * Restrictions: - can't handle the difference between int and long
 * 
 */
public class RpcStubGenerator {

	public static String generate(Class clazz, boolean onlyDeclared) {
		return generate(clazz.getName(), clazz.getClassLoader(), onlyDeclared);
	}

	public static String generate(Class clazz, ClassLoader cl,
			boolean onlyDeclared) {
		return generate(clazz.getName(), cl, onlyDeclared);
	}

	public static String generate(String className, ClassLoader cl,
			boolean onlyDeclared) {
		try {
			Class clazz = Class.forName(className, true, cl);

			String moduleName = module(clazz);

			return generate(clazz, moduleName, onlyDeclared);
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
			return e.getMessage();
		}
	}

	@SuppressWarnings("boxing")
	private static String generate(Class clazz, String moduleName,
			boolean onlyDeclared) {
		StringBuffer buf = new StringBuffer();
		buf.append(String.format("-module(%s).%n-compile(export_all).%n%n",
				moduleName));

		// TODO add constructors
		Constructor[] constructors = onlyDeclared ? clazz
				.getDeclaredConstructors() : clazz.getConstructors();
		Map<Integer, List<Constructor>> cmap = new HashMap<Integer, List<Constructor>>();
		for (Constructor constructor : constructors) {
			int plen = constructor.getParameterTypes().length;
			List<Constructor> list = cmap.get(plen);
			if (list == null) {
				list = new ArrayList<Constructor>();
			}
			list.add(constructor);
			cmap.put(plen, list);
		}

		for (Integer key : cmap.keySet()) {
			List<Constructor> list = cmap.get(key);
			Collections.sort(list, new Comparator<Constructor>() {

				public int compare(Constructor m1, Constructor m2) {
					Class<?>[] p1 = m1.getParameterTypes();
					Class<?>[] p2 = m2.getParameterTypes();
					for (int i = 0; i < p1.length; i++) {
						Class<?> t1 = RpcStubGenerator.javaType2erlang(p1[i]);
						Class<?> t2 = RpcStubGenerator.javaType2erlang(p2[i]);

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
			for (Constructor constructor : list) {
				printClause(clazz, buf, constructor);
				if (list.indexOf(constructor) == list.size() - 1) {
					buf.append(".\n\n");
				} else {
					buf.append(";\n");
				}
			}
		}

		Method[] methods = onlyDeclared ? clazz.getDeclaredMethods() : clazz
				.getMethods();

		Map<Tuple, List<Method>> mmap = new HashMap<Tuple, List<Method>>();
		for (Method method : methods) {
			int mod = method.getModifiers();
			boolean statik = Modifier.isStatic(mod);
			int plen = method.getParameterTypes().length;
			Tuple key = new Tuple().add(method.getName()).add(
					statik ? plen : plen + 1);
			List<Method> list = mmap.get(key);
			if (list == null) {
				list = new ArrayList<Method>();
			}
			list.add(method);
			mmap.put(key, list);
		}

		for (Tuple key : mmap.keySet()) {
			List<Method> list = mmap.get(key);
			Collections.sort(list, new Comparator<Method>() {

				public int compare(Method m1, Method m2) {
					Class<?>[] p1 = getExParams(m1);
					Class<?>[] p2 = getExParams(m2);
					for (int i = 0; i < p1.length; i++) {
						Class<?> t1 = RpcStubGenerator.javaType2erlang(p1[i]);
						Class<?> t2 = RpcStubGenerator.javaType2erlang(p2[i]);

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
			for (Method method : list) {
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

	private static void printClause(Class clazz, StringBuffer buf,
			Constructor constructor) {
		Class<?>[] params = constructor.getParameterTypes();

		buf.append("  'new'(");

		printParams(true, buf, params);
		StringBuffer guards = new StringBuffer();
		Class<?>[] p = constructor.getParameterTypes();
		for (int i = 0; i < p.length; i++) {
			String name = "P" + i;
			String grd = mkGuard(RpcStubGenerator.javaType2erlang(p[i]), name);
			guards.append(grd);
			if ((i < p.length - 1) && grd.length() > 0) {
				guards.append(", ");
			}
		}
		if (guards.length() != 0) {
			guards = new StringBuffer("when ").append(guards);
		}
		buf.append(") " + guards + " ->\n");

		buf.append("    jrpc:call(");
		buf.append("<<\"" + clazz.getName() + "\">>, ");

		StringBuffer args = new StringBuffer();
		Class<?>[] at = constructor.getParameterTypes();
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

	private static Class<?>[] getExParams(Method m1) {
		Class<?>[] p1 = m1.getParameterTypes();
		int mod = m1.getModifiers();
		boolean statik = Modifier.isStatic(mod);
		if (!statik) {
			p1 = new Class<?>[p1.length + 1];
			p1[0] = m1.getDeclaringClass();
			System.arraycopy(m1.getParameterTypes(), 0, p1, 1, p1.length - 1);
		}
		return p1;
	}

	private static void printClause(Class clazz, StringBuffer buf, Method method) {
		int mod = method.getModifiers();
		boolean statik = Modifier.isStatic(mod);
		Class<?>[] params = method.getParameterTypes();

		buf.append("  '" + method.getName() + "'(");
		if (!statik) {
			buf.append("Obj");
		}

		printParams(statik, buf, params);
		Class<?>[] p = getExParams(method);
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
				buf.append("    jrpc:cast(");
			} else {
				buf.append("    %% ");
				buf.append(" returns " + method.getReturnType().getName()
						+ "\n");
				buf.append("    jrpc:call(");
			}
			if (statik) {
				buf.append("<<\"" + clazz.getName() + "\">>, ");
			} else {
				buf.append("Obj, ");
			}
			StringBuffer args = new StringBuffer();
			Class<?>[] at = method.getParameterTypes();
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

	private static String mkGuard(Class<?> param, String name) {
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

	private static void printParams(boolean nohdr, StringBuffer buf,
			Class<?>[] params) {
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

	public static String module(Class clazz) {
		return clazz.getName().replaceAll("\\.", "_");
	}

	public static Class<?> javaType2erlang(Class<?> obj) {
		if (obj.isArray()) {
			return OtpErlangTuple.class;
		}
		if (List.class.isAssignableFrom(obj)) {
			return OtpErlangList.class;
		}
		if (obj == Integer.TYPE) {
			return OtpErlangLong.class;
		}
		if (obj == Long.TYPE) {
			return OtpErlangLong.class;
		}
		if (obj == Boolean.TYPE) {
			return OtpErlangAtom.class;
		}
		if (obj == Double.TYPE) {
			return OtpErlangDouble.class;
		}
		if (obj == String.class) {
			return OtpErlangString.class;
		}
		if (obj == Long.class) {
			return OtpErlangLong.class;
		}
		if (obj == Integer.class) {
			return OtpErlangLong.class;
		}
		if (obj == Double.class) {
			return OtpErlangDouble.class;
		}
		if (obj == Boolean.class) {
			return OtpErlangAtom.class;
		}
		return OtpErlangRef.class;

	}

}
