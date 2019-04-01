package org.erlide.runtime.internal;

import java.net.Socket;

import org.erlide.util.ErlLogger;

public class EpmdChecker {

    private static final int MAX_RETRIES = 30;
    private static final long RETRY_MILLIS = 100;
    private static int EPMD_PORT = Integer
            .parseInt(System.getProperty("erlide.epmd.port", "4369"));

    private int numAttempts = 0;
    private boolean done = false;

    private String host;

    EpmdChecker(String host) {
        this.host = host;
    }

    public void initialize() {
        Thread thread = new Thread(() -> {
            while (!done && numAttempts < MAX_RETRIES) {
                try {
                    numAttempts++;
                    try (Socket s = new Socket(host, EPMD_PORT)) {
                        s.close();
                    }
                    done = true;
                } catch (Exception e) {
                    try {
                        Thread.sleep(RETRY_MILLIS);
                    } catch (InterruptedException ie) {
                        // nop
                    }
                }
            }

            if (!done && numAttempts >= MAX_RETRIES) {
                String msg = "Couldn't contact epmd - erlang backend is probably not working."
                        + " Your host's entry in /etc/hosts is probably wrong (" + host
                        + ").";
                ErlLogger.error(msg);
                throw new RuntimeException(msg);
            }
        });
        thread.start();
    }

}
