package org.erlide.runtime.backend.internal;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Vector;

import org.eclipse.debug.core.DebugPlugin;

/**
 * Writes to the input stream of a system process, queueing output if the stream
 * is blocked.
 * 
 * The input stream monitor writes to system in via an output stream.
 */
public class InputStreamMonitor {

	/**
	 * The stream which is being written to (connected to system in).
	 */
	private final OutputStream fStream;
	/**
	 * The queue of output.
	 */
	private final Vector<String> fQueue;
	/**
	 * The thread which writes to the stream.
	 */
	private Thread fThread;
	/**
	 * A lock for ensuring that writes to the queue are contiguous
	 */
	private final Object fLock;

	/**
	 * Whether the underlying output stream has been closed
	 */
	private boolean fClosed = false;

	/**
	 * Creates an input stream monitor which writes to system in via the given
	 * output stream.
	 * 
	 * @param stream
	 *            output stream
	 */
	public InputStreamMonitor(final OutputStream stream) {
		fStream = stream;
		fQueue = new Vector<String>();
		fLock = new Object();
	}

	/**
	 * Appends the given text to the stream, or queues the text to be written at
	 * a later time if the stream is blocked.
	 * 
	 * @param text
	 *            text to append
	 */
	public void write(final String text) {
		synchronized (fLock) {
			fQueue.add(text);
			fLock.notifyAll();
		}
	}

	/**
	 * Starts a thread which writes the stream.
	 */
	public void startMonitoring() {
		if (fThread == null) {
			fThread = new Thread(new Runnable() {
				public void run() {
					write();
				}
			}, "InputStreamMonitor");
			fThread.setDaemon(true);
			fThread.start();
		}
	}

	/**
	 * Close all communications between this monitor and the underlying stream.
	 */
	public void close() {
		if (fThread != null) {
			final Thread thread = fThread;
			fThread = null;
			thread.interrupt();
		}
	}

	/**
	 * Continuously writes to the stream.
	 */
	protected void write() {
		while (fThread != null) {
			writeNext();
		}
		if (!fClosed) {
			try {
				fStream.close();
			} catch (final IOException e) {
				DebugPlugin.log(e);
			}
		}
	}

	/**
	 * Write the text in the queue to the stream.
	 */
	protected void writeNext() {
		while (!fQueue.isEmpty() && !fClosed) {
			final String text = fQueue.firstElement();
			fQueue.removeElementAt(0);
			try {
				fStream.write(text.getBytes());
				fStream.flush();
			} catch (final IOException e) {
				DebugPlugin.log(e);
			}
		}
		try {
			synchronized (fLock) {
				fLock.wait();
			}
		} catch (final InterruptedException e) {
		}
	}

	/**
	 * Closes the output stream attached to the standard input stream of this
	 * monitor's process.
	 * 
	 * @exception IOException
	 *                if an exception occurs closing the input stream
	 */
	public void closeInputStream() throws IOException {
		if (!fClosed) {
			fClosed = true;
			fStream.close();
		} else {
			throw new IOException();
		}

	}
}
