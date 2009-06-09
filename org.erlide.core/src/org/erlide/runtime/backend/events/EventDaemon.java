package org.erlide.runtime.backend.events;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendListener;
import org.erlide.jinterface.backend.BackendNode;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;

public class EventDaemon implements BackendListener {

	Backend runtime = null;
	volatile boolean fStopJob = false;
	List<EventHandler> handlers = new ArrayList<EventHandler>();
	final Object handlersLock = new Object();

	boolean DEBUG = "true".equals(System.getProperty("erlide.event.daemon"));

	private final class HandlerJob extends Job {
		HandlerJob(final String name) {
			super(name);
		}

		@Override
		protected IStatus run(final IProgressMonitor monitor) {
			try {
				OtpErlangObject msg = null;
				do {
					try {
						msg = runtime.receiveEvent(5);
						if (msg != null) {
							if (DEBUG) {
								ErlLogger.debug("MSG: %s", msg);
							}
							synchronized (handlersLock) {
								for (final EventHandler handler : handlers) {
									handler.handleMsg(msg);
								}
							}
						}
					} catch (final OtpErlangExit e) {
						if (!runtime.isStopped()) {
							// backend crashed -- restart?
							ErlLogger.warn(e);
						}
					} catch (final Exception e) {
						ErlLogger.warn(e);
					}
				} while (!fStopJob);
				return Status.OK_STATUS;
			} finally {
				synchronized (handlersLock) {
					handlers.clear();
				}
			}
		}
	}

	public EventDaemon(final Backend b) {
		runtime = b;
	}

	public synchronized void start() {
		fStopJob = false;

		ErlangCore.getBackendManager().addBackendListener(this);
		addHandler(new RpcHandler(runtime));

		final Job handlerJob = new HandlerJob("Erlang event daemon");
		handlerJob.setSystem(true);
		handlerJob.setPriority(Job.SHORT);
		handlerJob.schedule();
	}

	public synchronized void stop() {
		fStopJob = true;
	}

	public void runtimeAdded(final BackendNode b) {
	}

	public void runtimeRemoved(final BackendNode b) {
		if (b == runtime) {
			stop();
			runtime = null;
		}
	}

	public List<EventHandler> getHandlers() {
		return Collections.unmodifiableList(handlers);
	}

	public void addHandler(final EventHandler l) {
		synchronized (handlersLock) {
			if (!handlers.contains(l)) {
				handlers.add(l);
			}
		}
	}

	public void removeHandler(final EventHandler l) {
		synchronized (handlersLock) {
			handlers.remove(l);
		}
	}
}
