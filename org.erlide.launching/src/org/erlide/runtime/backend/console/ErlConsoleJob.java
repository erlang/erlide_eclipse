package org.erlide.runtime.backend.console;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.erlide.runtime.ErlangLaunchPlugin;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.IBackendListener;

import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlConsoleJob implements IBackendListener {

	private IBackend fBackend;

	private boolean fStopJob = false;

	private List<IErlConsoleListener> listeners;

	public ErlConsoleJob(IBackend b) {
		fBackend = b;
		listeners = new ArrayList<IErlConsoleListener>(10);

		BackendManager.getDefault().addBackendListener(this);

		Job handlerJob = new Job("erlang event router") {
			private List<OtpErlangObject> msgs = new ArrayList<OtpErlangObject>(
					10);

			@Override
			protected IStatus run(IProgressMonitor monitor) {
				try {
					msgs.clear();
					OtpErlangObject msg = null;
					do {
						try {
							msg = fBackend.receiveEvent(1);
							if (msg != null)
								msgs.add(msg);
						} catch (final OtpErlangException e) {
							ErlangLaunchPlugin.log(e);
							e.printStackTrace();
						}
					} while (msg != null && !fStopJob);
					notifyListeners(msgs);
					return Status.OK_STATUS;
				} finally {
					schedule(200);
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

	public void addListener(IErlConsoleListener client) {
		synchronized (listeners) {
			listeners.add(client);
		}
	}

	public void removeListener(IErlConsoleListener client) {
		synchronized (listeners) {
			listeners.remove(client);
		}
	}

	public void notifyListeners(List<OtpErlangObject> msgs) {
		if (msgs.size() == 0)
			return;
		synchronized (listeners) {
			for (IErlConsoleListener client : listeners) {
				client.handleEvent(msgs);
			}
		}
	}
}
