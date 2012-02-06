package org.erlide.launch.debug;

import java.util.Collection;

import org.erlide.jinterface.ErlLogger;
import org.erlide.launch.debug.model.ErlangDebugTarget;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;

class DebugEventHandler {

    private final ErlangDebugTarget debugTarget;

    DebugEventHandler(final ErlangDebugTarget erlangDebugTarget) {
        debugTarget = erlangDebugTarget;
    }

    public void handleMessages(final Collection<OtpErlangObject> messages)
            throws Exception {
        for (final OtpErlangObject message : messages) {
            handleMessage(message);
        }
    }

    private void handleMessage(final OtpErlangObject message) throws Exception {
        // TODO More events from erlide_dbg_mon...

        // System.out.println("@@@>> " + message);
        final OtpErlangTuple t = (OtpErlangTuple) message;
        final OtpErlangObject el0 = t.elementAt(0);
        if (el0 instanceof OtpErlangAtom) {
            final OtpErlangAtom a = (OtpErlangAtom) el0;
            final String tag = a.atomValue();
            if ("started".equals(tag)) {
                debugTarget.started();
            } else if ("terminated".equals(tag)) {
                debugTarget.terminate();
            } else if ("int".equals(tag)) {
                debugTarget.handleIntEvent((OtpErlangTuple) t.elementAt(1));
            } else if ("attached".equals(tag)) {
                final OtpErlangPid pid = (OtpErlangPid) t.elementAt(1);
                if (debugTarget.getMetaFromPid(pid) == null) {
                    final OtpErlangPid self = debugTarget.getEventMBox();
                    final OtpErlangPid metaPid = ErlideDebug.attached(
                            debugTarget.getBackend(), pid, self);
                    ErlLogger.debug("attached: " + pid + ",  meta: " + metaPid);
                    if (metaPid != null) {
                        debugTarget.putMetaPid(metaPid, pid);
                    }
                    // ErlideDebug.tracing(runtime, true, metaPid);
                }
            } else {
                // ErlLogger.debug("other event: " + msg);
            }
        } else if (el0 instanceof OtpErlangPid) { // meta event
            final OtpErlangPid pid = (OtpErlangPid) el0;
            final OtpErlangObject metaEvent = t.elementAt(1);
            if (metaEvent instanceof OtpErlangTuple) {
                final OtpErlangTuple metaEventTuple = (OtpErlangTuple) metaEvent;
                debugTarget.handleMetaEvent(pid, metaEventTuple);
            }
        }
    }

}
