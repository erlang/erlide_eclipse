package org.erlide.runtime.backend.events;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.Backend;
import org.erlide.runtime.backend.BackendListener;

import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;

public class EventDaemon implements BackendListener {

	Backend fBackend = null;
	volatile boolean fStopJob = false;
	List<EventHandler> fListeners = new ArrayList<EventHandler>();
	final Object listenersLock = new Object();

	boolean DEBUG = "true".equals(System.getProperty("erlide.event.daemon"));

	private final class HandlerJob extends Job {
		HandlerJob(String name) {
			super(name);
		}

		@Override
		protected IStatus run(IProgressMonitor monitor) {
			try {
				OtpErlangObject msg = null;
				do {
					try {
						msg = fBackend.receiveRpc(5);
						if (msg != null) {
							if (DEBUG) {
								ErlLogger.debug("MSG: %s", msg);
							}
							synchronized (listenersLock) {
								for (EventHandler handler : fListeners) {
									handler.handleMsg(msg);
								}
							}
						}
					} catch (final OtpErlangExit e) {
						// backend crashed -- restart?
						ErlLogger.warn(e);
					} catch (final Exception e) {
						ErlLogger.warn(e);
					}
				} while (!fStopJob);
				return Status.OK_STATUS;
			} finally {
				synchronized (listenersLock) {
					fListeners.clear();
				}
			}
		}
	}

	public EventDaemon(final Backend b) {
		fBackend = b;
	}

	public synchronized void start() {
		fStopJob = false;

		ErlangCore.getBackendManager().addBackendListener(this);
		addListener(new RpcHandler(fBackend));

		final Job handlerJob = new HandlerJob("Erlang event daemon");
		handlerJob.setSystem(true);
		handlerJob.setPriority(Job.SHORT);
		handlerJob.schedule();
	}

	public synchronized void stop() {
		fStopJob = true;
	}

	public void backendAdded(final Backend b) {
	}

	public void backendRemoved(final Backend b) {
		if (b == fBackend) {
			stop();
			fBackend = null;
		}
	}

	public List<EventHandler> getListeners() {
		return Collections.unmodifiableList(fListeners);
	}

	public void addListener(final EventHandler l) {
		synchronized (listenersLock) {
			if (!fListeners.contains(l)) {
				fListeners.add(l);
			}
		}
	}

	public void removeListener(final EventHandler l) {
		synchronized (listenersLock) {
			fListeners.remove(l);
		}
	}
}
