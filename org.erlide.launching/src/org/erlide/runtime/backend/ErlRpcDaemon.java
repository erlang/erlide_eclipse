package org.erlide.runtime.backend;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.erlide.basiccore.ErlLogger;
import org.erlide.jinterface.ErlUtils;
import org.erlide.runtime.ErlangLaunchPlugin;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlRpcDaemon implements IBackendListener {

	private static final boolean VERBOSE = true;

	// batch at most this many messages at once
	protected static final int MAX_RECEIVED = 10;

	private IBackend fBackend;

	private boolean fStopJob = false;

	public ErlRpcDaemon(IBackend b) {
		fBackend = b;

		BackendManager.getDefault().addBackendListener(this);

		Job handlerJob = new Job("Erlang RPC daemon") {
			private List<OtpErlangObject> msgs = new ArrayList<OtpErlangObject>(
					10);

			@Override
			protected IStatus run(IProgressMonitor monitor) {
				try {
					msgs.clear();
					OtpErlangObject msg = null;
					int received = 0;
					do {
						try {
							msg = fBackend.receiveRpc(1);
							if (msg != null) {
								received++;
								msgs.add(msg);
							}
						} catch (final OtpErlangException e) {
							ErlangLaunchPlugin.log(e);
							e.printStackTrace();
						}
					} while (msg != null && !fStopJob
							&& received < MAX_RECEIVED);
					if (msgs.size() > 0) {
						handleRequests(msgs);
					}
					return Status.OK_STATUS;
				} finally {
					if (ErlangLaunchPlugin.getDefault().getBundle().getState() != Bundle.STOPPING) {
						schedule(100);
					}
				}
			}
		};
		handlerJob.setSystem(true);
		handlerJob.setPriority(Job.SHORT);
		handlerJob.schedule();
	}

	public void stop() {
		fStopJob = true;
	}

	public void backendAdded(IBackend b) {
	}

	public void backendRemoved(IBackend b) {
		if (b == fBackend) {
			stop();
		}
	}

	public void handleRequests(List<OtpErlangObject> msgs) {
		if (msgs.size() == 0) {
			return;
		}
		for (OtpErlangObject msg : msgs) {
			try {
				OtpErlangTuple t = (OtpErlangTuple) msg;
				if (VERBOSE) {
					ErlLogger.log("-RPC: " + msg);
				}
				OtpErlangAtom kind = (OtpErlangAtom) t.elementAt(0);
				if ("call".equals(kind.atomValue())) {
					final OtpErlangPid from = (OtpErlangPid) t.elementAt(1);
					final OtpErlangObject target = t.elementAt(2);
					final OtpErlangAtom method = (OtpErlangAtom) t.elementAt(3);
					final OtpErlangList args = buildArgs(t.elementAt(4));
					Job job = new Job("rpc") {
						@Override
						protected IStatus run(IProgressMonitor monitor) {
							OtpErlangObject result = execute(target, method,
									args.elements());
							fBackend.send(from, new OtpErlangTuple(
									new OtpErlangObject[] {
											new OtpErlangAtom("ok"), result }));
							return Status.OK_STATUS;
						}
					};
					job.setSystem(true);
					job.setPriority(Job.SHORT);
					job.schedule();

				} else if ("cast".equals(kind.atomValue())) {
					final OtpErlangObject receiver = t.elementAt(1);
					final OtpErlangAtom method = (OtpErlangAtom) t.elementAt(2);
					final OtpErlangList args = buildArgs(t.elementAt(3));
					Job job = new Job("rpc") {
						@Override
						protected IStatus run(IProgressMonitor monitor) {
							execute(receiver, method, args.elements());
							return Status.OK_STATUS;
						}
					};
					job.setSystem(true);
					job.setPriority(Job.SHORT);
					job.schedule();

				} else if ("event".equals(kind.atomValue())) {
					String id = ((OtpErlangAtom) t.elementAt(1)).atomValue();
					List<IBackendEventListener> list = fBackend
							.getEventListeners(id);
					if (list != null) {
						for (IBackendEventListener client : list) {
							client.eventReceived(t.elementAt(2));
						}
					}
				} else {
					ErlLogger.log("RPC: unknown message type: " + msg);
				}
			} catch (Exception e) {
				ErlLogger.log("RPC: strange message: " + msg);
				e.printStackTrace();
			}
		}
	}

	private OtpErlangList buildArgs(OtpErlangObject a) throws Exception {
		final OtpErlangList args;
		if (a instanceof OtpErlangList) {
			args = (OtpErlangList) a;
		} else if (a instanceof OtpErlangString) {
			String ss = ((OtpErlangString) a).stringValue();
			byte[] bytes = ss.getBytes();
			OtpErlangObject[] str = new OtpErlangObject[ss.length()];
			for (int i = 0; i < ss.length(); i++) {
				str[i] = new OtpErlangInt(bytes[i]);
			}
			args = new OtpErlangList(str);
		} else {
			throw new Exception("bad RPC argument list: " + a);
		}
		return args;
	}

	private OtpErlangObject execute(OtpErlangObject target,
			OtpErlangAtom method, OtpErlangObject[] args) {

		if (VERBOSE) {
			ErlLogger.log("EXEC:: " + target + ":" + method + " " + args + " >"
					+ (args == null ? 0 : args.length));
		}

		Object[] parms;
		if (args != null) {
			parms = new Object[args.length];
			for (int i = 0; i < args.length; i++) {
				// parms[i] = args[i];
				parms[i] = ErlUtils.erlang2java(args[i]);
			}
		} else {
			parms = null;
		}

		if (target instanceof OtpErlangRef) {
			// object call
			Object rcvr = ErlUtils.getTarget((OtpErlangRef) target);
			if (rcvr != null) {
				try {
					return callMethod(rcvr, method.atomValue(), parms);
				} catch (Exception e) {
					ErlLogger.log("RPC: bad RPC: " + e.getMessage());
					return new OtpErlangTuple(new OtpErlangObject[] {
							new OtpErlangAtom("error"),
							new OtpErlangString(String.format("Bad RPC: %s", e
									.getMessage())) });
				}

			} else {
				ErlLogger.log("RPC: unknown receiver: " + target);
				return new OtpErlangTuple(new OtpErlangObject[] {
						new OtpErlangAtom("error"),
						new OtpErlangString(String.format(
								"Bad RPC: unknown object ref %s%n", target)) });
			}

		} else if (target instanceof OtpErlangAtom) {
			// static call
			String clazzName = ((OtpErlangAtom) target).atomValue();
			try {
				Class clazz = Class.forName(clazzName);
				return callMethod(clazz, method.atomValue(), parms);
			} catch (Exception e) {
				ErlLogger.log("RPC: bad RPC: " + e.getMessage());
				return new OtpErlangTuple(new OtpErlangObject[] {
						new OtpErlangAtom("error"),
						new OtpErlangString(String.format("Bad RPC: %s", e
								.getMessage())) });
			}
		} else {
			ErlLogger.log("RPC: unknown receiver: " + target);
			return new OtpErlangTuple(new OtpErlangObject[] {
					new OtpErlangAtom("error"),
					new OtpErlangString(String.format(
							"Bad RPC: unknown receiver %s", target)) });
		}
	}

	private OtpErlangObject callMethod(Object rcvr, String method, Object[] args)
			throws Exception {
		Class cls = (rcvr instanceof Class) ? (Class) rcvr : rcvr.getClass();
		Method meth; // = invoke(cls, args);
		boolean many;
		try {
			Class[] params;
			if (args != null) {
				params = new Class[args.length];
				for (int i = 0; i < args.length; i++) {
					// params[i] = Class
					// .forName("com.ericsson.otp.erlang.OtpErlangObject");
					params[i] = args[i].getClass();
					// params[i] =ErlUtils.erlang2javaType(args[i]);
					System.out.println("? " + i + " "
							+ args[i].getClass().getName() + " : "
							+ params[i].getName());
				}
			} else {
				params = null;
			}
			meth = cls.getMethod(method, params);
			many = false;
		} catch (NoSuchMethodException e) {
			try {
				Class[] params;
				if (args != null) {
					params = new Class[args.length];
					for (int i = 0; i < args.length; i++) {
						params[i] = Class.forName("java.lang.Object");
						// params[i] = args[i].getClass();
						// params[i] =ErlUtils.erlang2javaType(args[i]);
						System.out.println("? " + i + " "
								+ args[i].getClass().getName() + " : "
								+ params[i].getName());
					}
				} else {
					params = null;
				}
				meth = cls.getMethod(method, params);
				many = false;
			} catch (NoSuchMethodException ee) {
				meth = cls.getMethod(method, Class
						.forName("[Ljava.lang.Object;"));
				many = true;
			}
		}
		try {
			// meth.setAccessible(true);
			System.out.println("¤¤ " + many + " " + meth.isVarArgs());
			Object o = many ? meth.invoke(rcvr, (Object) args) : meth.invoke(
					rcvr, args);
			if (VERBOSE) {
				ErlLogger.log(String.format("** %s() returned %s", meth, o));
			}

			return ErlUtils.java2erlang(o);

			// Handle any exceptions thrown by method to be invoked.
		} catch (InvocationTargetException x) {
			Throwable cause = x.getCause();
			ErlLogger.log(String.format("invocation of %s failed: %s", meth,
					cause.getMessage()));
			return new OtpErlangTuple(new OtpErlangObject[] {
					new OtpErlangAtom("error"),
					new OtpErlangString(String.format(
							"invocation of %s failed: %s", meth, cause
									.getMessage())) });
		}
	}

	// /////////////////////// test rpc from erlang
	public static Object testing() {
		HashMap<String, String> m = new HashMap<String, String>();
		m.put("alfa", "beta");
		return m;
	}

	public static Object[] testing(Object arg1) {
		return new Object[] { 64, "hej", arg1 };
	}

	public static String testing(String arg1) {
		return arg1 + arg1;
	}

	public static Object testing(Object arg1, Object arg2) {
		return arg1;
	}

	public static Object testing(Object... args) {
		return args;
	}

	// /////////////////////////////////////////////////

}
