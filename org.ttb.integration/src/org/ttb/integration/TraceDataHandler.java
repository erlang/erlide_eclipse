package org.ttb.integration;

import java.util.Date;

import org.ttb.integration.mvc.model.CollectedData;
import org.ttb.integration.mvc.model.CollectedDataRoot;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;

/**
 * Handler which receives trace data from traced node. It receives data via
 * given {@link OtpMbox}.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TraceDataHandler {

    private final OtpMbox otpMbox;

    /**
     * Creates handler instance.
     * 
     * @param otpMbox
     *            mailbox used for receiving data
     */
    public TraceDataHandler(OtpMbox otpMbox) {
        this.otpMbox = otpMbox;
    }

    /**
     * Returns collected data.
     * 
     * @return collected data
     */
    public CollectedDataRoot getData() {
        CollectedDataRoot collectedDataRoot = new CollectedDataRoot("trace result: " + new Date());
        while (true) {
            try {
                OtpErlangObject otpErlangObject = otpMbox.receive();
                System.out.println("received: " + otpErlangObject);
                if (otpErlangObject instanceof OtpErlangAtom) {
                    OtpErlangAtom atom = (OtpErlangAtom) otpErlangObject;
                    if (atom.atomValue().equals("stop")) {
                        return collectedDataRoot;
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
    }
}
