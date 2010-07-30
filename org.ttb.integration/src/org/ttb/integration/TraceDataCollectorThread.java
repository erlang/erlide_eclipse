package org.ttb.integration;

import org.ttb.integration.mvc.model.CollectedData;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpMbox;

/**
 * Thread that collects traces via {@link OtpMbox} . Trace should be sent from
 * handler function given as one of arguments to the function
 * <code>ttb:format/2</code>.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TraceDataCollectorThread extends Thread {

    private final OtpMbox otpMbox;
    private boolean repeat = true;
    private final CollectedData collectedData;

    /**
     * Creates thread that will be receiving traces.
     * 
     * @param otpMbox
     *            process mailbox used to receive data from erlang node
     */
    public TraceDataCollectorThread(OtpMbox otpMbox) {
        this.otpMbox = otpMbox;
        this.collectedData = new CollectedData();
    }

    @Override
    public void run() {
        System.out.println("thread started");
        while (repeat) {
            try {
                OtpErlangObject otpErlangObject = otpMbox.receive();
                System.out.println("received: " + otpErlangObject);
                if (otpErlangObject instanceof OtpErlangAtom) {
                    OtpErlangAtom atom = (OtpErlangAtom) otpErlangObject;
                    if (atom.atomValue().equals("stop")) {
                        repeat = false;
                    }
                }
            } catch (Exception e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        System.out.println("thread ended");
    }

    public CollectedData getCollectedData() {
        return collectedData;
    }

    public OtpMbox getOtpMbox() {
        return otpMbox;
    }
}
