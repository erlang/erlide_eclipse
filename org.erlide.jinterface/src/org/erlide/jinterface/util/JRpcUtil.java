/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.jinterface.util;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlang;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.SignatureException;

public class JRpcUtil {
    static final String REF_NODE = "jRPC";

    // eclipse uses different classloaders for each plugin. this one is non-ui
    // so we have to set it from a ui one (when that one is initialized) so that
    // we can access even the UI classes (which are actually most interesting)
    public static ClassLoader loader = JRpcUtil.class.getClassLoader();

    public static OtpErlangObject execute(final OtpErlangObject target,
            final OtpErlangObject method, final OtpErlangObject[] args) {

        debug("EXEC:: " + target + ":" + method + " " + Arrays.toString(args)
                + " >" + (args == null ? 0 : args.length));

        final MethodDescription description = getDescription(method);

        Object[] parms;
        if (args != null) {
            parms = new Object[args.length];
            for (int i = 0; i < args.length; i++) {
                try {
                    parms[i] = TypeConverter.erlang2java(args[i],
                            description.argTypes[i]);
                } catch (final SignatureException e) {
                    e.printStackTrace();
                }
            }
        } else {
            parms = null;
        }

        if (target instanceof OtpErlangRef) {
            // object call
            final Object rcvr = ObjRefCache.getTarget((OtpErlangRef) target);
            if (rcvr != null) {
                try {
                    return callMethod(rcvr, description, parms);
                } catch (final Exception e) {
                    log("bad RPC 1: " + e.getMessage());
                    return makeErrorTuple(e);
                }

            }
            log("RPC: unknown receiver: " + target);
            return OtpErlang.mkTuple(
                    new OtpErlangAtom("error"),
                    new OtpErlangString(String.format(
                            "Bad RPC: unknown object ref %s%n", target)));

        } else if (target instanceof OtpErlangAtom
                || target instanceof OtpErlangString
                || target instanceof OtpErlangBinary) {
            // static call
            final String clazzName = ErlUtils.asString(target);
            try {
                final Class<?> clazz = Class.forName(clazzName, true, loader);
                return callMethod(clazz, description, parms);
            } catch (final Exception e) {
                log("bad RPC 2: " + e.getClass() + " " + e.getMessage());
                e.printStackTrace();
                return makeErrorTuple(e);
            }
        } else {
            log("unknown receiver: " + target);
            return OtpErlang.mkTuple(
                    new OtpErlangAtom("error"),
                    new OtpErlangString(String.format(
                            "Bad RPC: unknown receiver %s", target)));
        }
    }

    private static OtpErlangTuple makeErrorTuple(final Exception e) {
        return OtpErlang.mkTuple(
                new OtpErlangAtom("error"),
                new OtpErlangString(
                        String.format("Bad RPC: %s", e.getMessage())));
    }

    @SuppressWarnings("unchecked")
    private static MethodDescription getDescription(OtpErlangObject target) {
        if (!(target instanceof OtpErlangTuple)) {
            target = OtpErlang.mkTuple(target, new OtpErlangList());
        }
        final OtpErlangTuple t = (OtpErlangTuple) target;
        final String name = ErlUtils.asString(t.elementAt(0));
        Object olist = null;
        try {
            olist = TypeConverter.erlang2java(t.elementAt(1), String[].class);
        } catch (final SignatureException e) {
            // can't fail for String
        }
        if (olist instanceof List) {
            final List<String> arglist = (List<String>) olist;
            final Class<?>[] args = new Class<?>[arglist.size()];
            for (int i = 0; i < args.length; i++) {
                final String arg = arglist.get(i);
                args[i] = TypeConverter.getClassByName(arg);
            }
            return new MethodDescription(name, args);
        } else if (olist instanceof Object[]) {
            final Object[] arglist = (Object[]) olist;
            final Class<?>[] args = new Class<?>[arglist.length];
            for (int i = 0; i < args.length; i++) {
                final String arg = (String) arglist[i];
                args[i] = TypeConverter.getClassByName(arg);
            }
            return new MethodDescription(name, args);
        } else {
            // TODO should throw exception
            return null;
        }
    }

    private static OtpErlangObject callMethod(final Object rcvr,
            final MethodDescription method, Object[] args) {
        final Class<?> cls = rcvr instanceof Class<?> ? (Class<?>) rcvr : rcvr
                .getClass();

        Class<?>[] params = null;
        if (args == null) {
            args = new Object[] {};
        }
        params = new Class[args.length];
        for (int i = 0; i < args.length; i++) {
            params[i] = args[i].getClass();
        }

        try {
            if (method.name.equals(cls.getName())) {
                Constructor<?> ctr;
                ctr = cls.getConstructor(method.argTypes);
                // meth.setAccessible(true);
                final Object o = ctr.newInstance(args);
                debug(String.format("** %s() returned %s", ctr, o));

                return TypeConverter.java2erlang(o, "x");
            }
            Method meth;
            meth = cls.getMethod(method.name, method.argTypes);
            // meth.setAccessible(true);
            final Object o = meth.invoke(rcvr, args);
            debug(String.format("** %s() returned %s", meth, o));

            return TypeConverter.java2erlang(o, "x");
        } catch (final NoSuchMethodException e) {
            final StringBuilder paramstr = new StringBuilder();
            for (final Class<?> param : params) {
                paramstr.append(param.getName()).append(',');
            }
            return OtpErlang.mkTuple(
                    new OtpErlangAtom("error"),
                    new OtpErlangString(String.format(
                            "can't find method %s of %s(%s)", method.name,
                            cls.getName(), paramstr)));
        } catch (final InvocationTargetException x) {
            final Throwable cause = x.getCause();
            log(String.format("invocation of %s failed: %s", method.name,
                    cause.getMessage()));
            return OtpErlang.mkTuple(
                    new OtpErlangAtom("error"),
                    new OtpErlangString(String.format(
                            "invocation of %s failed: %s", method.name,
                            cause.getMessage())));
        } catch (final IllegalArgumentException x) {
            final StringBuilder paramstr = new StringBuilder();
            for (final Class<?> param : params) {
                paramstr.append(param.getName()).append(',');
            }
            log(String.format("invocation of %s failed: %s -- %s", method.name,
                    x.getMessage(), paramstr));
            return OtpErlang.mkTuple(
                    new OtpErlangAtom("error"),
                    new OtpErlangString(String.format(
                            "invocation of %s failed: %s", method.name,
                            x.getMessage())));
        } catch (final InstantiationException e) {
            final StringBuilder paramstr = new StringBuilder();
            for (final Class<?> param : params) {
                paramstr.append(param.getName()).append(',');
            }
            log(String.format("instantiation of %s failed: %s -- %s",
                    cls.getName(), e.getMessage(), paramstr));
            return OtpErlang.mkTuple(
                    new OtpErlangAtom("error"),
                    new OtpErlangString(String.format(
                            "invocation of %s failed: %s", cls.getName(),
                            e.getMessage())));
        } catch (final IllegalAccessException e) {
            ErlLogger.error(e);
            return null;
        } catch (final SignatureException e) {
            ErlLogger.error(e);
            return null;
        }

    }

    /**
     * This is a generic alternative to callMethod. Tests if an argument is
     * assignable to the declared method's type. It isn't yet adapted to
     * java-rpc.<br/>
     * Based on the paper at
     * http://www.jgroups.org/javagroupsnew/docs/papers/MethodResolution.ps.gz
     * 
     * @param message
     * @param target
     * @param args
     * @return
     */
    @SuppressWarnings({ "unused", "unchecked" })
    private static Object sendMessage(final String message,
            final Object target, final Object[] args) {
        try {
            // Is this an argumentless method call?
            if (args == null) {
                // Get the method.
                return target.getClass().getMethod(message, (Class[]) null)
                        .invoke(target, (Object[]) null);
            }
            // Get all methods from the target.
            final Method[] allMethods = target.getClass().getMethods();
            final List<Method> candidateMethods = new ArrayList<Method>();

            for (int i = 0; i < allMethods.length; i++) {
                // Filter methods by name and length of arguments.
                final Method m = allMethods[i];
                if (m.getName().equals(message)
                        && m.getParameterTypes().length == args.length) {
                    candidateMethods.add(m);
                }
            }

            if (candidateMethods.size() == 0) {
                throw new RuntimeException("");
            }

            Method callableMethod = null;
            for (final Iterator<Method> itr = candidateMethods.iterator(); itr
                    .hasNext();) {
                boolean callable = true;
                final Method m = itr.next();
                @SuppressWarnings("rawtypes")
                final Class[] argFormalTypes = m.getParameterTypes();
                for (int i = 0; i < argFormalTypes.length; i++) {
                    if (!argFormalTypes[i].isAssignableFrom(args[i].getClass())) {
                        callable = false;
                    }
                }
                if (callable) {
                    callableMethod = m;
                }
            }

            if (callableMethod != null) {
                return callableMethod.invoke(target, args);
            }
            throw new RuntimeException("No such method found: " + message);
        } catch (final Exception e) {
            final StringBuffer sb = new StringBuffer();
            // Build a helpful message to debug reflection issues.
            try {
                sb.append("ERROR: Could not send message '" + message
                        + "' to target of type " + target.getClass().toString()
                        + " \n");

                sb.append("\ttarget implements : \n");
                @SuppressWarnings("rawtypes")
                final Class[] interfaces = target.getClass().getInterfaces();
                for (int j = 0; j < interfaces.length; j++) {
                    sb.append("\t\t" + interfaces[j].getName() + "\n");
                }
                sb.append('\n');

                sb.append("\ttarget methods: \n");
                final Method[] methods = target.getClass().getMethods();
                for (int j = 0; j < methods.length; j++) {
                    sb.append("\t\t" + methods[j].getName() + "\n");
                }
                sb.append('\n');

                if (args != null) {
                    sb.append("\tArgument types: \n");
                    for (int j = 0; j < args.length; j++) {
                        sb.append("\t\t" + args[j].getClass().getName() + "\n");
                    }
                }
            } catch (final Exception e2) {
                throw new RuntimeException(
                        "ERROR: Could not create detailed error message for failed sendMessage() call.");
            }
            throw new RuntimeException(sb.toString());
        }

    }

    private static void log(final String s) {
        ErlLogger.info("JRpcUtil: " + s);
    }

    private static void debug(final String s) {
        ErlLogger.debug("JRpcUtil: " + s);
    }

    @SuppressWarnings("unused")
    private static void warn(final Exception e) {
        log(e.getMessage());
        e.printStackTrace();
    }

    private static class MethodDescription {
        public MethodDescription(final String meth, final Class<?>[] args) {
            name = meth;
            argTypes = args;
        }

        String name;
        Class<?>[] argTypes;
    }

    private JRpcUtil() {
    }
}
