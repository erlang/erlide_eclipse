package org.erlide.runtime.backend;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.erlide.basiccore.ErlLogger;
import org.erlide.jinterface.rpc.IRpcHandler;
import org.erlide.jinterface.rpc.RpcUtil;
import org.erlide.runtime.ErlangLaunchPlugin;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlRpcDaemon implements IBackendListener, IRpcHandler {

	// batch at most this many messages at once
	protected static final int MAX_RECEIVED = 10;

	IBackend fBackend;

	boolean fStopJob = false;

	public ErlRpcDaemon(IBackend b) {
		fBackend = b;

		BackendManager.getDefault().addBackendListener(this);

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
						} catch (final OtpErlangException e) {
							ErlangLaunchPlugin.log(e);
							e.printStackTrace();
						}
					} while (msg != null && !fStopJob
							&& received < MAX_RECEIVED);
					if (msgs.size() > 0) {
						RpcUtil.handleRequests(msgs, ErlRpcDaemon.this);
					}
					return Status.OK_STATUS;
				} finally {
					ErlangLaunchPlugin plugin = ErlangLaunchPlugin.getDefault();
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

	public void backendAdded(IBackend b) {
	}

	public void backendRemoved(IBackend b) {
		if (b == fBackend) {
			stop();
		}
	}

	public void rpcEvent(String id, OtpErlangObject event) {
		if ("log".equals(id)) {
			ErlLogger.debug("%s: %s", id, event.toString());
		} else if ("erlang_log".equals(id)) {
			final OtpErlangTuple t = (OtpErlangTuple) event;
			final OtpErlangAtom module = (OtpErlangAtom) t.elementAt(0);
			final OtpErlangLong line = (OtpErlangLong) t.elementAt(1);
			final OtpErlangAtom level = (OtpErlangAtom) t.elementAt(2);
			final OtpErlangObject logEvent = t.elementAt(3);
			try {
				ErlLogger.erlangLog(module.atomValue() + ".erl", line
						.uIntValue(), ErlLogger
						.levelFromName(level.atomValue()), "%s", logEvent
						.toString());
			} catch (final OtpErlangRangeException e) {
				e.printStackTrace();
			}
		}
		final List<IBackendEventListener> list = fBackend.getEventListeners(id);
		if (list != null) {
			for (final IBackendEventListener client : list) {
				client.eventReceived(event);
			}
		}
	}

	public void rpcReply(OtpErlangPid from, OtpErlangObject result) {
		fBackend.send(from, new OtpErlangTuple(new OtpErlangAtom("reply"),
				result));
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
}
