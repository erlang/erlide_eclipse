package org.erlide.jinterface.backend.events;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.Executor;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendListener;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;

public class EventDaemon implements BackendListener {

	Backend runtime = null;
	volatile boolean stopped = false;
	List<EventHandler> handlers = new ArrayList<EventHandler>();
	final Object handlersLock = new Object();
	private final static Executor executor = new ThreadPoolExecutor(1, 4, 5,
			TimeUnit.SECONDS, new ArrayBlockingQueue<Runnable>(5));

	boolean DEBUG = true;// "true".equals(System.getProperty("erlide.event.daemon"));

	private final class HandlerJob implements Runnable {
		public void run() {
			try {
				OtpErlangObject msg = null;
				final List<OtpErlangObject> msgs = new ArrayList<OtpErlangObject>();
				do {
					try {
						msg = runtime.receiveEvent(10);
						if (msg != null) {
							if (DEBUG) {
								ErlLogger.debug("MSG: %s", msg);
							}
							msgs.add(msg);
							// if there are more queued events, retrieve them
							do {
								msg = runtime.receiveEvent(0);
								if (msg != null) {
									if (DEBUG) {
										ErlLogger.debug("MSG: %s", msg);
									}
									msgs.add(msg);
								}
							} while (msg != null && !stopped);
						}
						if (msgs.size() != 0) {
							synchronized (handlersLock) {
								for (final EventHandler handler : handlers) {
									handler.handleMsgs(msgs);
								}
								msgs.clear();
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
				} while (!stopped);
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
		stopped = false;
		executor.execute(new HandlerJob());
		addHandler(new RpcHandler(runtime, executor));
	}

	public synchronized void stop() {
		stopped = true;
	}

	public void runtimeAdded(final Backend b) {
	}

	public void runtimeRemoved(final Backend b) {
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
