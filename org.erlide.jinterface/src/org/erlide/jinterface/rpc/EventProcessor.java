package org.erlide.jinterface.rpc;

import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;

public final class EventProcessor implements Runnable {

    private static final int TIMEOUT = 10000;

    private final EventHandler handler;
    private final OtpMbox mbox;
    private volatile boolean terminated = true;

    private OtpErlangPid starter;

    public EventProcessor(final EventHandler h, final Backend backend) {
        handler = h;
        mbox = backend.createMbox();
    }

    public void run() {
        terminated = false;
        mbox.send(starter, new OtpErlangAtom("start"));
        while (!terminated) {
            OtpErlangObject msg;
            try {
                msg = mbox.receive(TIMEOUT);
                if (msg != null) {
                    terminated = handler.handleEvent(msg);
                }
            } catch (final OtpErlangExit e) {
                terminated = true;
            } catch (final Exception e) {
                ErlLogger.warn("EventProcessor %s got exception:", mbox.self());
                ErlLogger.warn(e);
            }

        }
    }

    public void start() {
        if (!terminated) {
            return;
        }
        final Thread t = new Thread(this);
        t.setPriority(3); // low
        t.setDaemon(true);
        t.start();
    }

    public void stop() {
        terminated = true;
    }

    public OtpErlangPid getPid() {
        return mbox.self();
    }

    public void setStarter(final OtpErlangPid starter) {
        this.starter = starter;
    }

}
