package org.ttb.integration;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import org.erlide.jinterface.util.ErlLogger;
import org.ttb.integration.mvc.model.treenodes.FunctionNode;
import org.ttb.integration.mvc.model.treenodes.ITreeNode;
import org.ttb.integration.mvc.model.treenodes.ModuleNode;
import org.ttb.integration.mvc.model.treenodes.TracingResultsNode;
import org.ttb.integration.mvc.model.treenodes.TreeNode;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;

/**
 * Handler which receives trace data from traced node. It receives data via given {@link OtpMbox}.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TraceDataHandler {

    private static final String ATOM_STOP_TRACING = "stop_tracing";
    private static final String ATOM_STOP_LOADING = "stop_loading";

    // tuple fields
    private static final int INDEX_PROCESS = 1;
    private static final int INDEX_PROCESS_PID = 0;
    private static final int INDEX_PROCESS_INFO = 1;
    private static final int INDEX_PROCESS_NODE = 2;

    private static final int INDEX_TRACE_TYPE = 2;

    private static final int INDEX_FUNCTION = 3;
    private static final int INDEX_FUNCTION_MODULE = 0;
    private static final int INDEX_FUNCTION_NAME = 1;
    private static final int INDEX_FUNCTION_ARGS = 2;

    private static final int INDEX_MESSAGE = 3;
    private static final int INDEX_REGNAME = 3;
    private static final int INDEX_REASON = 3;
    private static final int INDEX_PROCESS2 = 3;
    private static final int INDEX_INFO = 3;

    private static final int INDEX_EXCEPTION = 4;
    private static final int INDEX_EXCEPTION_CLASS = 0;
    private static final int INDEX_EXCEPTION_VALUE = 1;

    private static final int INDEX_TO = 4;
    private static final int INDEX_RETURN_VALUE = 4;
    private static final int INDEX_SPAWN_FUNCTION = 4;

    private Date lastTraceDate;
    private String lastProcessDescription;
    private final SimpleDateFormat rootDateFormatter = new SimpleDateFormat("dd.MM.yy HH:mm:ss");
    private final SimpleDateFormat nodeDateFormatter = new SimpleDateFormat("HH:mm:ss.SSS dd.MM.yy");
    private String lastFunctionDescription;

    /**
     * Returns date formatter for formating date in root's label.
     * 
     * @return date formatter
     */
    public SimpleDateFormat getRootDateFormatter() {
        return rootDateFormatter;
    }

    /**
     * Return date of last trace that was passed to handler.
     * 
     * @return date
     */
    public Date getLastTraceDate() {
        return lastTraceDate;
    }

    public boolean isTracingFinished(OtpErlangObject message) {
        if (message instanceof OtpErlangAtom) {
            OtpErlangAtom atom = (OtpErlangAtom) message;
            if (atom.atomValue().equals(ATOM_STOP_TRACING)) {
                return true;
            }
        }
        return false;
    }

    public boolean isLoadingFinished(OtpErlangObject message) {
        if (message instanceof OtpErlangAtom) {
            OtpErlangAtom atom = (OtpErlangAtom) message;
            if (atom.atomValue().equals(ATOM_STOP_LOADING)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Creates root of tree displaying tracing results. Label of created node is set to display date of last trace data which was passed to this handler (it is
     * returned by {@link #getLastTraceDate()}). Root's start date is also set to this date.
     * 
     * @return tree root
     */
    public TracingResultsNode createRoot() {
        TracingResultsNode node = new TracingResultsNode(rootDateFormatter.format(lastTraceDate));
        node.setStartDate(lastTraceDate);
        return node;
    }

    /**
     * Returns collected data.
     * 
     * @return collected data
     */
    public ITreeNode getData(OtpErlangObject otpErlangObject) {
        try {
            if (otpErlangObject instanceof OtpErlangTuple) {
                OtpErlangTuple tuple = (OtpErlangTuple) otpErlangObject;
                OtpErlangAtom traceType = (OtpErlangAtom) tuple.elementAt(INDEX_TRACE_TYPE);

                lastTraceDate = readTraceData(tuple);

                switch (TraceType.valueOf(traceType.atomValue().toUpperCase())) {
                case CALL:
                    return processCallTrace("Call", tuple);
                case EXCEPTION_FROM:
                    return processExceptionFrom("Exception", tuple);
                case EXIT:
                    return processExitTrace("Exit", tuple);
                case GC_END:
                    return processGcTrace("GC end", Images.GC_END_NODE, tuple);
                case GC_START:
                    return processGcTrace("GC start", Images.GC_START_NODE, tuple);
                case GETTING_LINKED:
                    return processLinkTrace("Getting linked", Images.GETTING_LINKED_NODE, tuple);
                case GETTING_UNLINKED:
                    return processLinkTrace("Getting unlinked", Images.GETTING_UNLINKED_NODE, tuple);
                case IN:
                    return processInOutTrace("In", Images.IN_NODE, tuple);
                case LINK:
                    return processLinkTrace("Link", Images.LINK_NODE, tuple);
                case OUT:
                    return processInOutTrace("Out", Images.OUT_NODE, tuple);
                case RECEIVE:
                    return processReceiveTrace("Received", tuple);
                case REGISTER:
                    return processRegisterTrace("Register", Images.REGISTER_NODE, tuple);
                case RETURN_FROM:
                    return processReturnTrace("Return from", Images.RETURN_FROM_NODE, tuple, true);
                case RETURN_TO:
                    return processReturnTrace("Return to", Images.RETURN_TO_NODE, tuple, false);
                case SEND:
                    return processSendTrace("Sent", Images.SENT_MESSAGE_NODE, tuple);
                case SEND_TO_NON_EXISTING_PROCESS:
                    return processSendTrace("Sent to non existing process", Images.WRONG_MESSAGE_NODE, tuple);
                case SPAWN:
                    return processSpawnTrace("Spawn", tuple);
                case UNLINK:
                    return processLinkTrace("Unlink", Images.ULINK_NODE, tuple);
                case UNREGISTER:
                    return processRegisterTrace("Unregister", Images.UNREGISTER_NODE, tuple);
                }
            }
        } catch (Exception e) {
            ErlLogger.error(e);
        }
        return null;
    }

    private Date readTraceData(OtpErlangTuple tuple) throws OtpErlangRangeException {
        OtpErlangTuple dateAndTimeTuple = (OtpErlangTuple) tuple.elementAt(tuple.arity() - 1);
        OtpErlangTuple dateTuple = (OtpErlangTuple) dateAndTimeTuple.elementAt(0);
        OtpErlangTuple timeTuple = (OtpErlangTuple) dateAndTimeTuple.elementAt(1);

        int year = ((OtpErlangLong) dateTuple.elementAt(0)).intValue();
        int month = ((OtpErlangLong) dateTuple.elementAt(1)).intValue() - 1;
        int day = ((OtpErlangLong) dateTuple.elementAt(2)).intValue();
        int hour = ((OtpErlangLong) timeTuple.elementAt(0)).intValue();
        int minute = ((OtpErlangLong) timeTuple.elementAt(1)).intValue();
        int second = ((OtpErlangLong) timeTuple.elementAt(2)).intValue();

        Calendar calendar = Calendar.getInstance();
        calendar.set(year, month, day, hour, minute, second);

        return calendar.getTime();
    }

    private String createLabel(String text) {
        return "[" + nodeDateFormatter.format(lastTraceDate) + "] " + text;
    }

    private String pid2Str(OtpErlangPid pid) {
        return new StringBuilder().append(pid.id()).append(".").append(pid.serial()).append(".").append(pid.creation()).toString();
    }

    // functions creating nodes

    private ITreeNode createProcessNode(String label, OtpErlangObject erlangObject) {
        StringBuilder builder = new StringBuilder();
        ITreeNode functionNode = null;
        ITreeNode nameNode = null;
        ITreeNode pidNode = null;
        ITreeNode processNodeNode = null;

        if (erlangObject instanceof OtpErlangTuple) {
            // tuple: {Pid(), Initial_call()|Registered_name(), Node()} | {Registered_name, Node()}
            OtpErlangTuple processTuple = (OtpErlangTuple) erlangObject;
            int index = 0;

            // pid
            OtpErlangPid pid = null;
            if (processTuple.arity() == 3) {
                // {Pid(), Initial_call()|Registered_name(), Node()}
                pid = (OtpErlangPid) processTuple.elementAt(INDEX_PROCESS_PID);
                pidNode = new TreeNode("pid: " + pid2Str(pid), Activator.getImage(Images.INFO_NODE));
            } else {
                index = 1;// tuple doesn't contain Pid element
            }

            OtpErlangObject info = processTuple.elementAt(INDEX_PROCESS_INFO - index);

            // process node
            OtpErlangObject processNode = processTuple.elementAt(INDEX_PROCESS_NODE - index);
            processNodeNode = new TreeNode("node: " + processNode, Activator.getImage(Images.INFO_NODE));

            if (info instanceof OtpErlangTuple) {
                // initial call
                functionNode = createFunctionNode("initial call:", info);
                functionNode.setImage(Activator.getImage(Images.INFO_NODE));
                if (pid != null) {
                    builder.append(pid2Str(pid));
                } else {
                    builder.append(lastFunctionDescription);
                }
            } else {
                // registered name
                nameNode = new TreeNode("name: " + info, Activator.getImage(Images.INFO_NODE));
                builder.append(info.toString());
            }
            builder.append(" (").append(processNode).append(")");
        } else if (erlangObject instanceof OtpErlangPid) {
            // Pid
            OtpErlangPid pid = (OtpErlangPid) erlangObject;
            pidNode = new TreeNode("pid: " + pid2Str(pid), Activator.getImage(Images.INFO_NODE));
            processNodeNode = new TreeNode("node: " + pid.node(), Activator.getImage(Images.INFO_NODE));
            builder.append(pid2Str(pid)).append(" (").append(pid.node()).append(")");
        } else {
            // Atom (registered name)
            nameNode = new TreeNode("name: " + erlangObject, Activator.getImage(Images.INFO_NODE));
            builder.append(erlangObject.toString());
        }

        ITreeNode node = new TreeNode();
        if (pidNode != null)
            node.addChildren(pidNode);
        if (nameNode != null)
            node.addChildren(nameNode);
        if (processNodeNode != null)
            node.addChildren(processNodeNode);
        if (functionNode != null)
            node.addChildren(functionNode);

        lastProcessDescription = builder.toString();
        node.setLabel(label + lastProcessDescription);

        return node;
    }

    private ITreeNode createFunctionNode(String label, OtpErlangObject erlangObject) {
        ITreeNode node = new TreeNode();
        if (erlangObject instanceof OtpErlangTuple) {
            OtpErlangTuple functionTuple = (OtpErlangTuple) erlangObject;
            OtpErlangAtom moduleName = (OtpErlangAtom) functionTuple.elementAt(INDEX_FUNCTION_MODULE);
            OtpErlangAtom functionName = (OtpErlangAtom) functionTuple.elementAt(INDEX_FUNCTION_NAME);

            // args or arity node
            TreeNode argsNode = new TreeNode();
            argsNode.setImage(Activator.getImage(Images.INFO_NODE));
            OtpErlangObject arityOrArgs = functionTuple.elementAt(INDEX_FUNCTION_ARGS);
            int arityValue = -1;
            if (arityOrArgs instanceof OtpErlangList) {
                // last element is a list of arguments
                OtpErlangList arguments = (OtpErlangList) arityOrArgs;
                StringBuilder builder = new StringBuilder("arguments: ");
                for (int i = 1; i < arguments.arity(); i++) {
                    builder.append(arguments.elementAt(i)).append(", ");
                }
                arityValue = arguments.arity() - 1;
                argsNode.setLabel(builder.substring(0, builder.length() - 2));
            } else {
                // last element is arity
                try {
                    if (functionTuple.elementAt(INDEX_FUNCTION_ARGS) instanceof OtpErlangInt) {
                        arityValue = ((OtpErlangInt) functionTuple.elementAt(INDEX_FUNCTION_ARGS)).intValue();
                    } else {
                        arityValue = (int) ((OtpErlangLong) functionTuple.elementAt(INDEX_FUNCTION_ARGS)).longValue();
                    }
                    argsNode.setLabel("arity: " + arityValue);
                } catch (OtpErlangRangeException e) {
                    ErlLogger.error(e);
                }
            }

            // module name node
            TreeNode moduleNameNode = new ModuleNode(moduleName.atomValue());
            moduleNameNode.setLabel("module: " + moduleName);

            // function name node
            TreeNode functionNameNode = new FunctionNode(moduleName.atomValue(), functionName.atomValue(), arityValue);
            functionNameNode.setLabel("function: " + functionName);

            node.addChildren(moduleNameNode, functionNameNode, argsNode);
            lastFunctionDescription = label + moduleName + ":" + functionName + "/" + arityValue;

        } else {
            lastFunctionDescription = "unknown";
        }
        node.setLabel(lastFunctionDescription);
        return node;
    }

    private ITreeNode createMessageNode(OtpErlangObject message) {
        ITreeNode node = new TreeNode(message.toString(), Activator.getImage(Images.MESSAGE_NODE));
        return node;
    }

    // functions processing different trace types

    private ITreeNode processGcTrace(String label, Images image, OtpErlangTuple tuple) {
        StringBuilder labelBuilder = new StringBuilder(label).append(": ");

        ITreeNode processNode = createProcessNode("process: ", tuple.elementAt(INDEX_PROCESS));
        processNode.setImage(Activator.getImage(Images.PROCESS_NODE));
        labelBuilder.append(lastProcessDescription);

        ITreeNode node = new TreeNode(createLabel(labelBuilder.toString()), Activator.getImage(image));
        node.addChildren(processNode);

        OtpErlangList list = (OtpErlangList) tuple.elementAt(INDEX_INFO);
        for (OtpErlangObject otpErlangObject : list.elements()) {
            OtpErlangTuple infoTuple = (OtpErlangTuple) otpErlangObject;
            OtpErlangObject key = infoTuple.elementAt(0);
            OtpErlangObject value = infoTuple.elementAt(1);
            TreeNode treeNode = new TreeNode(key.toString() + ": " + value.toString());
            treeNode.setImage(Activator.getImage(Images.INFO_NODE));
            node.addChildren(treeNode);
        }
        return node;
    }

    private ITreeNode processSpawnTrace(String label, OtpErlangTuple tuple) {
        StringBuilder labelBuilder = new StringBuilder(label).append(": ");

        ITreeNode processNode = createProcessNode("process: ", tuple.elementAt(INDEX_PROCESS));
        processNode.setImage(Activator.getImage(Images.PROCESS_NODE));
        labelBuilder.append(lastProcessDescription).append("->");

        ITreeNode processNode2 = createProcessNode("new process: ", tuple.elementAt(INDEX_PROCESS2));
        processNode2.setImage(Activator.getImage(Images.NEW_PROCESS_NODE));
        labelBuilder.append(lastProcessDescription);

        ITreeNode functionNode = createFunctionNode("function: ", tuple.elementAt(INDEX_SPAWN_FUNCTION));
        functionNode.setImage(Activator.getImage(Images.FUNCTION_NODE));

        ITreeNode node = new TreeNode(createLabel(labelBuilder.toString()), Activator.getImage(Images.SPAWN_NODE));
        node.addChildren(processNode, processNode2, functionNode);
        return node;
    }

    private ITreeNode processExceptionFrom(String label, OtpErlangTuple tuple) {
        StringBuilder labelBuilder = new StringBuilder(label).append(": ");

        ITreeNode processNode = createProcessNode("process: ", tuple.elementAt(INDEX_PROCESS));
        processNode.setImage(Activator.getImage(Images.PROCESS_NODE));

        OtpErlangTuple exceptionTuple = (OtpErlangTuple) tuple.elementAt(INDEX_EXCEPTION);
        OtpErlangObject exceptionClass = exceptionTuple.elementAt(INDEX_EXCEPTION_CLASS);
        OtpErlangObject exceptionValue = exceptionTuple.elementAt(INDEX_EXCEPTION_VALUE);
        labelBuilder.append(exceptionClass.toString());

        ITreeNode exceptionClassNode = new TreeNode(exceptionClass.toString(), Activator.getImage(Images.INFO_NODE));
        exceptionClassNode.addChildren(new TreeNode(exceptionValue.toString()));

        ITreeNode node = processReturnTrace(labelBuilder.toString(), Images.EXCEPTION_NODE, tuple, false);
        node.addChildren(processNode, exceptionClassNode);
        return node;
    }

    private ITreeNode processReturnTrace(String label, Images image, OtpErlangTuple tuple, boolean showRetValue) {
        StringBuilder labelBuilder = new StringBuilder(label).append(": ");

        ITreeNode processNode = createProcessNode("process: ", tuple.elementAt(INDEX_PROCESS));
        processNode.setImage(Activator.getImage(Images.PROCESS_NODE));

        ITreeNode functionNode = createFunctionNode("function: ", tuple.elementAt(INDEX_FUNCTION));
        functionNode.setImage(Activator.getImage(Images.FUNCTION_NODE));
        labelBuilder.append(lastFunctionDescription);

        ITreeNode node = new TreeNode();
        node.addChildren(processNode, functionNode);

        if (showRetValue) {
            ITreeNode returnValueNode = new TreeNode("return value: " + tuple.elementAt(INDEX_RETURN_VALUE));
            returnValueNode.setImage(Activator.getImage(Images.INFO_NODE));
            labelBuilder.append("->").append(tuple.elementAt(INDEX_RETURN_VALUE));
            node.addChildren(returnValueNode);
        }

        node.setLabel(createLabel(labelBuilder.toString()));
        node.setImage(Activator.getImage(image));
        return node;
    }

    private ITreeNode processInOutTrace(String label, Images image, OtpErlangTuple tuple) {
        StringBuilder labelBuilder = new StringBuilder(label).append(": ");

        ITreeNode processNode = createProcessNode("process: ", tuple.elementAt(INDEX_PROCESS));
        processNode.setImage(Activator.getImage(Images.PROCESS_NODE));
        labelBuilder.append(lastProcessDescription);

        ITreeNode functionNode = createFunctionNode("function: ", tuple.elementAt(INDEX_FUNCTION));
        functionNode.setImage(Activator.getImage(Images.FUNCTION_NODE));

        ITreeNode node = new TreeNode(createLabel(labelBuilder.toString()), Activator.getImage(image));
        node.addChildren(processNode, functionNode);
        return node;
    }

    private ITreeNode processRegisterTrace(String label, Images image, OtpErlangTuple tuple) {
        StringBuilder labelBuilder = new StringBuilder(label).append(": ");

        ITreeNode process = createProcessNode("process: ", tuple.elementAt(INDEX_PROCESS));
        process.setImage(Activator.getImage(Images.REGISTER_NODE));
        labelBuilder.append(lastProcessDescription).append("->");

        TreeNode regName = new TreeNode("name: " + tuple.elementAt(INDEX_REGNAME).toString(), Activator.getImage(Images.INFO_NODE));
        labelBuilder.append(regName);

        ITreeNode node = new TreeNode(createLabel(labelBuilder.toString()), Activator.getImage(image));
        node.addChildren(process, regName);
        return node;
    }

    private ITreeNode processLinkTrace(String label, Images image, OtpErlangTuple tuple) {
        StringBuilder labelBuilder = new StringBuilder(label).append(": ");

        ITreeNode process1Node = createProcessNode("process 1: ", tuple.elementAt(INDEX_PROCESS));
        process1Node.setImage(Activator.getImage(Images.PROCESS_NODE));
        labelBuilder.append(lastProcessDescription).append("->");

        ITreeNode process2Node = createProcessNode("process 2: ", tuple.elementAt(INDEX_PROCESS2));
        process2Node.setImage(Activator.getImage(Images.PROCESS_NODE));
        labelBuilder.append(lastProcessDescription);

        TreeNode node = new TreeNode(createLabel(labelBuilder.toString()));
        node.setImage(Activator.getImage(image));
        node.addChildren(process1Node, process2Node);
        return node;
    }

    private ITreeNode processExitTrace(String label, OtpErlangTuple tuple) {
        StringBuilder labelBuilder = new StringBuilder(label).append(": ");

        ITreeNode processNode = createProcessNode("process: ", tuple.elementAt(INDEX_PROCESS));
        processNode.setImage(Activator.getImage(Images.PROCESS_NODE));
        labelBuilder.append(lastProcessDescription);

        ITreeNode reasonNode = new TreeNode("reason: " + tuple.elementAt(INDEX_REASON).toString(), Activator.getImage(Images.INFO_NODE));

        TreeNode node = new TreeNode(createLabel(labelBuilder.toString()));
        node.setImage(Activator.getDefault().getImageRegistry().get(Images.EXIT_NODE.toString()));
        node.addChildren(processNode, reasonNode);
        return node;
    }

    private ITreeNode processReceiveTrace(String label, OtpErlangTuple tuple) {
        StringBuilder labelBuilder = new StringBuilder(label).append(": ");

        ITreeNode processNode = createProcessNode("receiver: ", tuple.elementAt(INDEX_PROCESS));
        processNode.setImage(Activator.getImage(Images.RECEIVER_NODE));
        labelBuilder.append(lastProcessDescription);

        ITreeNode messageNode = createMessageNode(tuple.elementAt(INDEX_MESSAGE));

        TreeNode node = new TreeNode(createLabel(labelBuilder.toString()));
        node.setImage(Activator.getDefault().getImageRegistry().get(Images.RECEIVED_MESSAGE_NODE.toString()));
        node.addChildren(processNode, messageNode);
        return node;
    }

    private ITreeNode processSendTrace(String label, Images image, OtpErlangTuple tuple) {
        StringBuilder labelBuilder = new StringBuilder(label).append(": ");

        ITreeNode senderNode = createProcessNode("sender: ", tuple.elementAt(INDEX_PROCESS));
        senderNode.setImage(Activator.getImage(Images.SENDER_NODE));
        labelBuilder.append(lastProcessDescription).append("->");

        ITreeNode receiverNode = createProcessNode("receiver: ", tuple.elementAt(INDEX_TO));
        receiverNode.setImage(Activator.getImage(Images.RECEIVER_NODE));
        labelBuilder.append(lastProcessDescription);

        ITreeNode messageNode = createMessageNode(tuple.elementAt(INDEX_MESSAGE));

        TreeNode node = new TreeNode(createLabel(labelBuilder.toString()));
        node.setImage(Activator.getImage(image));
        node.addChildren(senderNode, receiverNode, messageNode);
        return node;
    }

    private ITreeNode processCallTrace(String label, OtpErlangTuple tuple) {
        ITreeNode processNode = createProcessNode("process: ", tuple.elementAt(INDEX_PROCESS));
        processNode.setImage(Activator.getImage(Images.PROCESS_NODE));

        ITreeNode node = createFunctionNode(createLabel(label + ": "), tuple.elementAt(INDEX_FUNCTION));
        node.setImage(Activator.getImage(Images.CALL_NODE));
        node.addChildren(processNode);
        return node;
    }
}
