package org.erlide.runtime.backend;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.JInterfaceFactory;
import org.erlide.jinterface.rpc.IRpcHandler;
import org.erlide.jinterface.rpc.RpcUtil;
import org.erlide.runtime.ErlLogger;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlRpcDaemon implements BackendListener, IRpcHandler {

	// batch at most this many messages at once
	protected static final int MAX_RECEIVED = 10;

	Backend fBackend = null;

	boolean fStopJob = false;

	public ErlRpcDaemon(final Backend b) {
		fBackend = b;
	}

	List<ErlRpcMessageListener> fErlRpcMessageListeners = new ArrayList<ErlRpcMessageListener>();

	public void start() {
		if (!fStopJob) {
			return;
		}
		ErlangCore.getBackendManager().addBackendListener(this);

		final Job handlerJob = new Job("Erlang RPC daemon") {
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
						} catch (final OtpErlangExit e) {
							// backend crashed -- restart?
							ErlLogger.warn(e);
						} catch (final OtpErlangException e) {
							ErlLogger.warn(e);
						}
					} while (msg != null && !fStopJob
							&& received < MAX_RECEIVED);
					for (ErlRpcMessageListener i : fErlRpcMessageListeners) {
						i.handleMsgs(msgs);
					}
					if (msgs.size() > 0) {
						RpcUtil.handleRequests(msgs, ErlRpcDaemon.this);
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
			ErlLogger.debug("%s", t.elementAt(1).toString());
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

	public void addErlRpcMessageListener(final ErlRpcMessageListener l) {
		fErlRpcMessageListeners.add(l);
	}

	public void removeErlRpcMessageListener(final ErlRpcMessageListener l) {
		fErlRpcMessageListeners.remove(l);
	}
}
