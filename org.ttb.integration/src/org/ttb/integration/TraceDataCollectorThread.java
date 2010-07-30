package org.ttb.integration;

import java.util.Date;

import org.ttb.integration.mvc.model.CollectedData;
import org.ttb.integration.mvc.model.CollectedDataList;
import org.ttb.integration.mvc.model.CollectedDataRoot;

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
    private final CollectedDataList collectedData;

    /**
     * Creates thread that will be receiving traces.
     * 
     * @param otpMbox
     *            process mailbox used to receive data from erlang node
     */
    public TraceDataCollectorThread(OtpMbox otpMbox) {
        this.otpMbox = otpMbox;
        this.collectedData = new CollectedDataList();
    }

    @Override
    public void run() {
        System.out.println("thread started");
        CollectedDataRoot collectedDataRoot = new CollectedDataRoot("trace result: " + new Date());
        while (repeat) {
            try {
                OtpErlangObject otpErlangObject = otpMbox.receive();
                System.out.println("received: " + otpErlangObject);
                if (otpErlangObject instanceof OtpErlangAtom) {
                    OtpErlangAtom atom = (OtpErlangAtom) otpErlangObject;
                    if (atom.atomValue().equals("stop")) {
                        collectedData.addData(collectedDataRoot);
                        repeat = false;
                    }
                } else {
                    collectedDataRoot.addChild(new CollectedData(otpErlangObject.toString()));
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        System.out.println("thread ended");
    }

    public CollectedDataList getCollectedData() {
        return collectedData;
    }

    public OtpMbox getOtpMbox() {
        return otpMbox;
    }
}
