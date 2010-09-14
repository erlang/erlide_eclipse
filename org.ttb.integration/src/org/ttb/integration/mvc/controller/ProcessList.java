package org.ttb.integration.mvc.controller;

import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.util.ErlLogger;
import org.ttb.integration.TraceBackend;
import org.ttb.integration.mvc.model.TracedProcess;
import org.ttb.integration.mvc.model.TracedNode;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Helper class for dealing with process lists.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class ProcessList {

    private static final String MODULE_NAME = "proc_list";
    private static final String FUNCTION_NAME = "list_all_processes";

    private ProcessList() {
    }

    /**
     * Returns list of processes on all traced nodes.
     * 
     * @return list of processes
     */
    public static TracedProcess[] getProcsOnTracedNodes() {
        try {
            Backend backend = TraceBackend.getInstance().getBackend();
            Object[] tracedNodes = TraceBackend.getInstance().getTracedNodesArray();
            OtpErlangAtom[] nodeAtoms = new OtpErlangAtom[tracedNodes.length];

            for (int i = 0; i < tracedNodes.length; i++) {
                nodeAtoms[i] = new OtpErlangAtom(((TracedNode) tracedNodes[i]).getNodeName());
            }
            OtpErlangList nodesList = new OtpErlangList(nodeAtoms);
            OtpErlangList procList = (OtpErlangList) backend.call(MODULE_NAME, FUNCTION_NAME, "x", nodesList);
            TracedProcess[] processes = new TracedProcess[procList.arity()];

            for (int i = 0; i < procList.arity(); i++) {
                OtpErlangTuple tuple = (OtpErlangTuple) procList.elementAt(i);
                processes[i] = new TracedProcess(tuple);
            }
            return processes;
        } catch (BackendException e) {
            ErlLogger.error(e);
        }
        return null;
    }
}
