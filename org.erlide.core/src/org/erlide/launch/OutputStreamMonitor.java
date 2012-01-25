package org.erlide.launch;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;

import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IStreamListener;
import org.eclipse.debug.core.model.IFlushableStreamMonitor;

/**
 * Monitors the output stream of a system process and notifies listeners of
 * additions to the stream.
 * 
 * The output stream monitor reads system out (or err) via and input stream.
 */
public class OutputStreamMonitor implements IFlushableStreamMonitor {
    /**
     * The stream being monitored (connected system out or err).
     */
    private final InputStream fStream;

    /**
     * A collection of listeners
     */
    ListenerList fListeners = new ListenerList();

    /**
     * Whether content is being buffered
     */
    private boolean fBuffered = true;

    /**
     * The local copy of the stream contents
     */
    private final StringBuffer fContents;

    /**
     * The thread which reads from the stream
     */
    private Thread fThread;

    /**
     * The size of the read buffer
     */
    private static final int BUFFER_SIZE = 8192;

    /**
     * Whether or not this monitor has been killed. When the monitor is killed,
     * it stops reading from the stream immediately.
     */
    private boolean fKilled = false;

    private long lastSleep;

    private final String fEncoding;

    /**
     * Creates an output stream monitor on the given stream (connected to system
     * out or err).
     * 
     * @param stream
     *            input stream to read from
     * @param encoding
     *            stream encoding or <code>null</code> for system default
     */
    public OutputStreamMonitor(final InputStream stream, final String encoding) {
        fStream = new BufferedInputStream(stream, 8192);
        fEncoding = encoding;
        fContents = new StringBuffer();
    }

    @Override
    public synchronized void addListener(final IStreamListener listener) {
        fListeners.add(listener);
    }

    /**
     * Causes the monitor to close all communications between it and the
     * underlying stream by waiting for the thread to terminate.
     */
    protected void close() {
        if (fThread != null) {
            final Thread thread = fThread;
            fThread = null;
            try {
                thread.join();
            } catch (final InterruptedException ie) {
            }
            fListeners = new ListenerList();
        }
    }

    /**
     * Notifies the listeners that text has been appended to the stream.
     */
    private void fireStreamAppended(final String text) {
        getNotifier().notifyAppend(text);
    }

    @Override
    public synchronized String getContents() {
        return fContents.toString();
    }

    /**
     * Continually reads from the stream.
     * <p>
     * This method, along with the <code>startReading</code> method is used to
     * allow <code>OutputStreamMonitor</code> to implement <code>Runnable</code>
     * without publicly exposing a <code>run</code> method.
     */
    void read() {
        lastSleep = System.currentTimeMillis();
        long currentTime = lastSleep;
        final byte[] bytes = new byte[BUFFER_SIZE];
        int read = 0;
        while (read >= 0) {
            try {
                if (fKilled) {
                    break;
                }
                read = fStream.read(bytes);
                if (read > 0) {
                    String text;
                    if (fEncoding != null) {
                        text = new String(bytes, 0, read, fEncoding);
                    } else {
                        text = new String(bytes, 0, read);
                    }
                    synchronized (this) {
                        if (isBuffered()) {
                            fContents.append(text);
                        }
                        fireStreamAppended(text);
                    }
                }
            } catch (final IOException ioe) {
                if (!fKilled) {
                    DebugPlugin.log(ioe);
                }
                return;
            } catch (final NullPointerException e) {
                // killing the stream monitor while reading can cause an NPE
                // when reading from the stream
                if (!fKilled && fThread != null) {
                    DebugPlugin.log(e);
                }
                return;
            }

            currentTime = System.currentTimeMillis();
            if (currentTime - lastSleep > 1000) {
                lastSleep = currentTime;
                try {
                    Thread.sleep(1); // just give up CPU to maintain UI
                    // responsiveness.
                } catch (final InterruptedException e) {
                }
            }
        }
        try {
            fStream.close();
        } catch (final IOException e) {
            DebugPlugin.log(e);
        }
    }

    protected void kill() {
        fKilled = true;
    }

    @Override
    public synchronized void removeListener(final IStreamListener listener) {
        fListeners.remove(listener);
    }

    /**
     * Starts a thread which reads from the stream
     */
    protected void startMonitoring() {
        if (!getNotifier().ensureLoaded()) {
            return;
        }
        if (fThread == null) {
            fThread = new Thread(new Runnable() {
                @Override
                public void run() {
                    read();
                }
            }, "OutputStreamMonitor");
            fThread.setDaemon(true);
            fThread.setPriority(Thread.MIN_PRIORITY);
            fThread.start();
        }
    }

    /**
     * @see org.eclipse.debug.core.model.IFlushableStreamMonitor#setBuffered(boolean)
     */
    @Override
    public synchronized void setBuffered(final boolean buffer) {
        fBuffered = buffer;
    }

    /**
     * @see org.eclipse.debug.core.model.IFlushableStreamMonitor#flushContents()
     */
    @Override
    public synchronized void flushContents() {
        fContents.setLength(0);
    }

    /**
     * @see IFlushableStreamMonitor#isBuffered()
     */
    @Override
    public synchronized boolean isBuffered() {
        return fBuffered;
    }

    private ContentNotifier getNotifier() {
        return new ContentNotifier();
    }

    class ContentNotifier implements ISafeRunnable {

        private IStreamListener fListener;
        private String fText;

        /**
         * @see org.eclipse.core.runtime.ISafeRunnable#handleException(java.lang.Throwable)
         */
        @Override
        public void handleException(final Throwable exception) {
            DebugPlugin.log(exception);
        }

        /**
         * @see org.eclipse.core.runtime.ISafeRunnable#run()
         */
        @Override
        public void run() throws Exception {
            fListener.streamAppended(fText, OutputStreamMonitor.this);
        }

        public void notifyAppend(final String text) {
            if (text == null) {
                return;
            }
            fText = text;
            final Object[] copiedListeners = fListeners.getListeners();
            for (int i = 0; i < copiedListeners.length; i++) {
                fListener = (IStreamListener) copiedListeners[i];
                SafeRunner.run(this);
            }
            fListener = null;
            fText = null;
        }

        public boolean ensureLoaded() {
            return true;
        }
    }
}
