package org.erlide.runtime.backend.events;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.Backend;
import org.erlide.runtime.backend.BackendListener;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;

public class EventDaemon implements BackendListener {

	Backend fBackend = null;
	volatile boolean fStopJob = false;
	List<EventHandler> fListeners = new ArrayList<EventHandler>();

	private final class HandlerJob extends Job {
		final private int MSG_BATCH_SIZE = 10;

		private HandlerJob(String name) {
			super(name);
		}

		@Override
		protected IStatus run(IProgressMonitor monitor) {
			try {
				OtpErlangObject msg = null;
				int received = 0;
				do {
					try {
						msg = fBackend.receiveRpc(5);
					} catch (final OtpErlangExit e) {
						// backend crashed -- restart?
						ErlLogger.warn(e);
					} catch (final OtpErlangException e) {
						ErlLogger.warn(e);
					}
					if (msg != null) {
						for (EventHandler handler : fListeners) {
							handler.handleMsg(msg);
						}
					}
				} while (msg != null && !fStopJob && received < MSG_BATCH_SIZE);
				return Status.OK_STATUS;
			} finally {
				ErlangPlugin plugin = ErlangPlugin.getDefault();
				if (plugin != null
						&& plugin.getBundle().getState() != Bundle.STOPPING
						&& !fStopJob) {
					schedule(50);
				} else {
					fListeners.clear();
				}
			}
		}
	}

	public EventDaemon(final Backend b) {
		fBackend = b;
	}

	public void start() {
		if (fStopJob) {
			return;
		}
		ErlangCore.getBackendManager().addBackendListener(this);
		addListener(new RpcHandler(fBackend));

		final Job handlerJob = new HandlerJob("Erlang event daemon");
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

	public List<EventHandler> getListeners() {
		return Collections.unmodifiableList(fListeners);
	}

	public void addListener(final EventHandler l) {
		if (!fListeners.contains(l)) {
			fListeners.add(l);
		}
	}

	public void removeListener(final EventHandler l) {
		fListeners.remove(l);
	}
}
