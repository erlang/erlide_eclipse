package org.erlide.runtime.backend;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.erlide.basiccore.ErlLogger;
import org.erlide.runtime.ErlangLaunchPlugin;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlRpcDaemon implements IBackendListener, IErlRpcWrapper {

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
					schedule(100);
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
				ErlLogger.log("RPC: " + msg);
				OtpErlangAtom kind = (OtpErlangAtom) t.elementAt(0);
				if ("call".equals(kind.atomValue())) {
					final OtpErlangPid from = (OtpErlangPid) t.elementAt(1);
					final OtpErlangObject receiver = t.elementAt(2);
					final OtpErlangAtom method = (OtpErlangAtom) t.elementAt(3);
					final OtpErlangList args = (OtpErlangList) t.elementAt(4);
					Job job = new Job("rpc") {
						@Override
						protected IStatus run(IProgressMonitor monitor) {
							OtpErlangObject[] args1 = args.elements();
							OtpErlangObject result = execute(receiver, method,
									args1);
							System.out.println("reply to " + from);
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
					final OtpErlangList args = (OtpErlangList) t.elementAt(3);
					Job job = new Job("rpc") {
						@Override
						protected IStatus run(IProgressMonitor monitor) {
							OtpErlangObject[] args1 = args.elements();
							execute(receiver, method, args1);
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

	private OtpErlangObject execute(OtpErlangObject receiver,
			OtpErlangAtom method, OtpErlangObject[] args) {

		ErlLogger.log("EXEC:: " + receiver + ":" + method + " " + args);

		if (receiver instanceof OtpErlangRef) {
			// object call

		} else if (receiver instanceof OtpErlangAtom) {
			// static call
			String clazzName = ((OtpErlangAtom) receiver).atomValue();
			try {
				Class clazz = Class.forName(clazzName);
				List<Type> intfs = Arrays.asList(clazz.getGenericInterfaces());
				if (intfs.contains(IErlRpcWrapper.class)) {
					Method meth = clazz
							.getMethod(
									method.atomValue(),
									Class
											.forName("[Lcom.ericsson.otp.erlang.OtpErlangObject;"));

					try {
						meth.setAccessible(true);
						Object o = meth.invoke(clazz, (Object) args);
						System.out.format("%s() returned %s%n", meth, o);

						if (o instanceof OtpErlangObject)
							return (OtpErlangObject) o;
						else if (o instanceof String)
							return new OtpErlangString((String) o);
						else
							return new OtpErlangString("???" + o.toString());

						// Handle any exceptions thrown by method to be invoked.
					} catch (InvocationTargetException x) {
						Throwable cause = x.getCause();
						System.out.format("invocation of %s failed: %s%n",
								meth, cause.getMessage());
					} catch (IllegalArgumentException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (IllegalAccessException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				} else {
					System.out
							.println("trying to call not supported object type");
				}
			} catch (ClassNotFoundException e) {
				e.printStackTrace();
			} catch (SecurityException e) {
				e.printStackTrace();
			} catch (NoSuchMethodException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

		} else {
			ErlLogger.log("RPC: bad receiver: " + receiver);
		}

		OtpErlangAtom error = new OtpErlangAtom("error");
		return error;
	}

	// test rpc from erlang
	public static OtpErlangObject testing(OtpErlangObject... args) {
		return new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangAtom("hej"), new OtpErlangList(args) });
	}

}
