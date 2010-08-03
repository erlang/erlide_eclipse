package org.ttb.integration;

import java.util.Date;

import org.ttb.integration.mvc.model.CollectedData;
import org.ttb.integration.mvc.model.CollectedDataList;
import org.ttb.integration.mvc.model.CollectedDataRoot;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
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
                    // TODO introduce Enum or constant describing indices
                    OtpErlangTuple tuple = (OtpErlangTuple) otpErlangObject;
                    OtpErlangAtom nodeName = (OtpErlangAtom) tuple.elementAt(0);
                    OtpErlangAtom moduleName = (OtpErlangAtom) tuple.elementAt(1);
                    OtpErlangAtom functionName = (OtpErlangAtom) tuple.elementAt(2);
                    OtpErlangList arguments = (OtpErlangList) tuple.elementAt(3);

                    // TODO add fields to CollectedData to describe trail
                    // details (not only label)
                    CollectedData data = new CollectedData(moduleName + ": " + functionName + "/" + (arguments.arity() - 1));
                    data.addChild(new CollectedData("module: " + moduleName));
                    data.addChild(new CollectedData("function: " + functionName));

                    StringBuilder builder = new StringBuilder("arguments: ");
                    for (int i = 1; i < arguments.arity(); i++) {
                        builder.append(arguments.elementAt(i)).append(", ");
                    }
                    data.addChild(new CollectedData(builder.substring(0, builder.length() - 2)));

                    collectedDataRoot.addChild(data);
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
