package org.erlide.tracing.core.mvc.controller;

import java.util.ArrayList;
import java.util.List;

import org.erlide.backend.IBackend;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.tracing.core.TraceBackend;
import org.erlide.tracing.core.mvc.model.TracedNode;
import org.erlide.tracing.core.mvc.model.TracedProcess;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Helper class for dealing with process lists.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class ProcessHelper {

    private static final String MODULE_NAME = "proc_list";
    private static final String FUNCTION_NAME = "list_all_processes";

    private ProcessHelper() {
    }

    /**
     * Returns list of processes on all traced nodes.
     * 
     * @return list of processes
     */
    public static TracedProcess[] getProcsOnTracedNodes() {
        try {
            final IBackend backend = TraceBackend.getInstance().getBackend(
                    true);
            final List<OtpErlangAtom> nodeAtoms = new ArrayList<OtpErlangAtom>();

            for (final Object o : TraceBackend.getInstance()
                    .getTracedNodesArray()) {
                final TracedNode tracedNode = (TracedNode) o;
                if (tracedNode.isEnabled()) {
                    nodeAtoms.add(new OtpErlangAtom(tracedNode.getNodeName()));
                }
            }

            final OtpErlangList nodesList = new OtpErlangList(
                    nodeAtoms.toArray(new OtpErlangAtom[nodeAtoms.size()]));
            final OtpErlangList procList = (OtpErlangList) backend.call(
                    MODULE_NAME, FUNCTION_NAME, "x", nodesList);
            final TracedProcess[] processes = new TracedProcess[procList
                    .arity()];

            for (int i = 0; i < procList.arity(); i++) {
                final OtpErlangTuple tuple = (OtpErlangTuple) procList
                        .elementAt(i);
                processes[i] = new TracedProcess(tuple);
            }
            return processes;
        } catch (final RpcException e) {
            ErlLogger.error(e);
        }
        return null;
    }
}
