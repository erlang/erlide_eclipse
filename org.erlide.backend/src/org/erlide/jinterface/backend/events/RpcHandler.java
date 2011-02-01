package org.erlide.jinterface.backend.events;

import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import org.erlide.jinterface.backend.Backend;

import com.ericsson.otp.erlang.OtpErlang;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;

public final class RpcHandler extends EventHandler {
    private final Backend fRuntime;

    private final Executor executor = Executors.newFixedThreadPool(1);

    public RpcHandler(final Backend runtime) {
        fRuntime = runtime;
    }

    @Override
    protected void doHandleEvent(final ErlangEvent event) throws Exception {
        // ErlLogger.debug("-- RPC: " + msg);

        // TODO use standard event format

        // if (msg instanceof OtpErlangTuple) {
        // final OtpErlangTuple t = (OtpErlangTuple) msg;
        // final OtpErlangObject e0 = t.elementAt(0);
        // if (e0 instanceof OtpErlangAtom) {
        // final OtpErlangAtom kind = (OtpErlangAtom) e0;
        // final OtpErlangObject receiver = t.elementAt(1);
        // final OtpErlangObject target = t.elementAt(2);
        // if ("call".equals(kind.atomValue())) {
        // final OtpErlangList args = buildArgs(t.elementAt(3));
        // final OtpErlangPid from = (OtpErlangPid) t.elementAt(4);
        // executor.execute(new Runnable() {
        // public void run() {
        // final OtpErlangObject result = JRpcUtil.execute(
        // receiver, target, args.elements());
        // rpcReply(from, result);
        // }
        // });
        //
        // } else if ("uicall".equals(kind.atomValue())) {
        // final OtpErlangPid from = (OtpErlangPid) t.elementAt(1);
        // final OtpErlangList args = buildArgs(t.elementAt(4));
        // // TODO how to mark this as executable in UI thread?
        // executor.execute(new Runnable() {
        // public void run() {
        // final OtpErlangObject result = JRpcUtil.execute(
        // receiver, target, args.elements());
        // rpcReply(from, result);
        // }
        // });
        //
        // } else if ("cast".equals(kind.atomValue())) {
        // final OtpErlangList args = buildArgs(t.elementAt(3));
        // executor.execute(new Runnable() {
        // public void run() {
        // JRpcUtil.execute(receiver, target, args.elements());
        // }
        // });
        // }
        // }
        // }
    }

    private static OtpErlangList buildArgs(final OtpErlangObject a)
            throws Exception {
        final OtpErlangList args;
        if (a instanceof OtpErlangList) {
            args = (OtpErlangList) a;
        } else if (a instanceof OtpErlangString) {
            final String ss = ((OtpErlangString) a).stringValue();
            final byte[] bytes = ss.getBytes();
            final OtpErlangObject[] str = new OtpErlangObject[ss.length()];
            for (int i = 0; i < ss.length(); i++) {
                str[i] = new OtpErlangInt(bytes[i]);
            }
            args = new OtpErlangList(str);
        } else {
            throw new Exception("bad RPC argument list: " + a);
        }
        return args;
    }

    public void rpcReply(final OtpErlangPid from, final OtpErlangObject result) {
        fRuntime.send(from,
                OtpErlang.mkTuple(new OtpErlangAtom("reply"), result));
    }

}
