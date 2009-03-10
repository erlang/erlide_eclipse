package org.erlide.runtime.backend.events;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.Bindings;
import org.erlide.jinterface.ErlUtils;
import org.erlide.jinterface.JInterfaceFactory;
import org.erlide.jinterface.ParserException;
import org.erlide.jinterface.rpc.IRpcHandler;
import org.erlide.jinterface.rpc.RpcUtil;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.Backend;
import org.erlide.runtime.backend.BackendEventListener;
import org.erlide.runtime.backend.BackendListener;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlRpcDaemon implements BackendListener, IRpcHandler {

	// batch at most this many messages at once
	protected static final int MAX_RECEIVED = 10;

	Backend fBackend = null;

	boolean fStopJob = false;

	public ErlRpcDaemon(final Backend b) {
		fBackend = b;
	}

	Map<String, List<EventListener>> fListeners = new HashMap<String, List<EventListener>>();

	public void start() {
		if (fStopJob) {
			return;
		}
		ErlangCore.getBackendManager().addBackendListener(this);

		final Job handlerJob = new Job("Erlang RPC daemon") {
			private List<OtpErlangObject> msgs = new ArrayList<OtpErlangObject>();

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
						} catch (final OtpErlangExit e) {
							// backend crashed -- restart?
							ErlLogger.warn(e);
						} catch (final OtpErlangException e) {
							ErlLogger.warn(e);
						}
					} while (msg != null && !fStopJob
							&& received < MAX_RECEIVED);
					if (msgs.size() > 0) {
						// for (OtpErlangObject mmm : msgs) {
						// if (mmm instanceof OtpErlangTuple) {
						// OtpErlangTuple tuple = (OtpErlangTuple) mmm;
						// if (tuple.arity() > 0) {
						// OtpErlangObject head = tuple.elementAt(0);
						// if (head instanceof OtpErlangAtom) {
						// String msgid = ((OtpErlangAtom) head)
						// .atomValue();
						// List<EventListener> listeners = getListeners(msgid);
						// if (listeners != null) {
						// for (EventListener i : listeners) {
						// if (i.handleMsg(msg)) {
						// continue;
						// }
						// }
						// }
						// }
						// }
						// }
						// }
						for (EventListener lll : getListeners("")) {
							lll.handleMsgs(msgs);
						}

						handleRequests(msgs);
					}
					return Status.OK_STATUS;
				} finally {
					ErlangPlugin plugin = ErlangPlugin.getDefault();
					if (plugin != null
							&& plugin.getBundle().getState() != Bundle.STOPPING) {
						schedule(50);
					}
				}
			}

		};
		handlerJob.setSystem(true);
		handlerJob.setPriority(Job.SHORT);
		handlerJob.schedule();
	}

	public void handleRequests(List<OtpErlangObject> msgs) {
		if (msgs.size() == 0) {
			return;
		}
		for (OtpErlangObject msg : msgs) {
			try {
				OtpErlangTuple t = (OtpErlangTuple) msg;
				// debug("-- RPC: " + msg);
				OtpErlangAtom kind = (OtpErlangAtom) t.elementAt(0);
				final OtpErlangObject receiver = t.elementAt(1);
				final OtpErlangObject target = t.elementAt(2);
				if ("call".equals(kind.atomValue())) {
					final OtpErlangList args = buildArgs(t.elementAt(3));
					final OtpErlangPid from = (OtpErlangPid) t.elementAt(4);
					executeRpc(new Runnable() {
						public void run() {
							OtpErlangObject result = RpcUtil.execute(receiver,
									target, args.elements());
							rpcReply(from, result);
						}
					});

				} else if ("uicall".equals(kind.atomValue())) {
					final OtpErlangPid from = (OtpErlangPid) t.elementAt(1);
					final OtpErlangList args = buildArgs(t.elementAt(4));
					// TODO how to mark this as executable in UI thread?
					executeRpc(new Runnable() {
						public void run() {
							OtpErlangObject result = RpcUtil.execute(receiver,
									target, args.elements());
							rpcReply(from, result);
						}
					});

				} else if ("cast".equals(kind.atomValue())) {
					final OtpErlangList args = buildArgs(t.elementAt(3));
					executeRpc(new Runnable() {
						public void run() {
							RpcUtil.execute(receiver, target, args.elements());
						}
					});

				} else if ("event".equals(kind.atomValue())) {
					final String id = ((OtpErlangAtom) receiver).atomValue();
					rpcEvent(id, target);
					// rpcHandler.executeRpc(new Runnable() {
					// public void run() {
					// rpcHandler.rpcEvent(id, target);
					// }
					// });

				} else {
					ErlLogger.error("unknown message type: " + msg);
				}
			} catch (Exception e) {
				ErlLogger.error("strange message: " + msg);
				e.printStackTrace();
			}
		}
	}

	private static OtpErlangList buildArgs(OtpErlangObject a) throws Exception {
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

	public void stop() {
		fStopJob = true;
	}

	public void backendAdded(final Backend b) {
	}

	public void backendRemoved(final Backend b) {
		if (b == fBackend) {
			stop();
		}
	}

	public void rpcEvent(final String id, final OtpErlangObject event) {
		if ("log".equals(id)) {
			final OtpErlangTuple t = (OtpErlangTuple) event;
			try {
				Bindings b = ErlUtils.match("{K:a,M,P:p}", t);
				String kind = ((OtpErlangAtom) b.get("K")).atomValue();
				OtpErlangObject msg = b.get("M");
				@SuppressWarnings("unused")
				OtpErlangPid pid = ((OtpErlangPid) b.get("P"));
				ErlLogger.debug("%s: %s", kind, msg);
			} catch (ParserException e) {
				ErlLogger.error(e);
			}
		} else if ("erlang_log".equals(id)) {
			final OtpErlangTuple t = (OtpErlangTuple) event;
			final OtpErlangAtom module = (OtpErlangAtom) t.elementAt(0);
			final OtpErlangLong line = (OtpErlangLong) t.elementAt(1);
			final OtpErlangAtom level = (OtpErlangAtom) t.elementAt(2);
			final OtpErlangObject logEvent = t.elementAt(3);
			String ss = "";
			if (t.arity() == 5) {
				final OtpErlangTuple backtrace_0 = (OtpErlangTuple) t
						.elementAt(4);
				final OtpErlangBinary backtrace = (OtpErlangBinary) backtrace_0
						.elementAt(1);
				ss = new String(backtrace.binaryValue());
			}
			try {
				ErlLogger.erlangLog(module.atomValue() + ".erl", line
						.uIntValue(), level.atomValue().toUpperCase(), "%s %s",
						logEvent.toString(), ss);
			} catch (final OtpErlangRangeException e) {
				ErlLogger.warn(e);
			}
		}
		final List<BackendEventListener> list = fBackend.getEventListeners(id);
		if (list != null) {
			for (final BackendEventListener client : list) {
				client.eventReceived(event);
			}
		}
	}

	public void rpcReply(final OtpErlangPid from, final OtpErlangObject result) {
		fBackend.send(from, JInterfaceFactory.mkTuple(
				new OtpErlangAtom("reply"), result));
	}

	public void executeRpc(final Runnable runnable) {
		final Job job = new Job("rpc") {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				runnable.run();
				return Status.OK_STATUS;
			}
		};
		job.setSystem(true);
		job.setPriority(Job.SHORT);
		job.schedule();
	}

	public void addListener(final EventListener l, String id) {
		List<EventListener> list = getListeners(id);
		if (!list.contains(list)) {
			list.add(l);
		}
		if (id.length() > 0) {
			addListener(l, "");
		}
	}

	public List<EventListener> getListeners(String id) {
		List<EventListener> list = fListeners.get(id);
		if (list == null) {
			list = new ArrayList<EventListener>();
			fListeners.put(id, list);
		}
		return list;
	}

	public void removeListener(final EventListener l, String id) {
		List<EventListener> list = getListeners(id);
		list.remove(l);
		if (id.length() > 0 && list.size() == 0) {
			removeListener(l, "");
		}
	}

	public void removeListener(final EventListener l) {
		Collection<List<EventListener>> list = fListeners.values();
		for (List<EventListener> onelist : list) {
			onelist.remove(l);
		}
	}
}
