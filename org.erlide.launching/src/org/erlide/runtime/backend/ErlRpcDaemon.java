package org.erlide.runtime.backend;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.erlide.basiccore.ErlLogger;
import org.erlide.runtime.ErlangLaunchPlugin;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlRpcDaemon implements IBackendListener {

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
					OtpErlangPid from = (OtpErlangPid) t.elementAt(1);
					OtpErlangObject receiver = t.elementAt(2);
					OtpErlangObject call = t.elementAt(3);

				} else if ("cast".equals(kind.atomValue())) {
					OtpErlangObject receiver = t.elementAt(1);
					OtpErlangObject call = t.elementAt(2);

				} else if ("event".equals(kind.atomValue())) {
					String id = ((OtpErlangAtom) t.elementAt(1)).atomValue();
					List<IBackendEventListener> list = fBackend
							.getEventListeners(id);
					for (IBackendEventListener client : list) {
						client.eventReceived(t.elementAt(2));
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
}
