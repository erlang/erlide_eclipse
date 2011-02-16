package org.erlide.tracing.core.mvc.model;

import java.util.HashSet;
import java.util.Set;

import org.erlide.tracing.core.ProcessFlag;

import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Traced process.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TracedProcess {

    // fields in tuple describing process
    private static final int PID = 0;
    private static final int NAME = 1;
    private static final int INITIAL_CALL = 2;
    private static final int NODE_NAME = 3;

    private boolean selected;
    private final String name;
    private final OtpErlangPid pid;
    private final String initialCall;
    private final String node;
    private final Set<ProcessFlag> flags = new HashSet<ProcessFlag>();

    /**
     * Creates process from tuple. List of tuples describing all processes is
     * returned as a result of <code>erlide_proclist:process_list/0</code> call.
     * 
     * @param tuple
     *            tuple describing process
     */
    public TracedProcess(final OtpErlangTuple tuple) {
        name = tuple.elementAt(NAME).toString();
        pid = (OtpErlangPid) tuple.elementAt(PID);
        initialCall = tuple.elementAt(INITIAL_CALL).toString();
        node = tuple.elementAt(NODE_NAME).toString();
    }

    public boolean isSelected() {
        return selected;
    }

    public void setSelected(final boolean selected) {
        this.selected = selected;
    }

    public String getName() {
        return name;
    }

    public OtpErlangPid getPid() {
        return pid;
    }

    public String getInitialCall() {
        return initialCall;
    }

    public String getNode() {
        return node;
    }

    public void setFlag(final ProcessFlag flag) {
        flags.add(flag);
    }

    public void unSetFlag(final ProcessFlag flag) {
        flags.remove(flag);
    }

    public boolean hasFlag(final ProcessFlag flag) {
        return flags.contains(flag);
    }

    public Set<ProcessFlag> getFlags() {
        return flags;
    }
}
