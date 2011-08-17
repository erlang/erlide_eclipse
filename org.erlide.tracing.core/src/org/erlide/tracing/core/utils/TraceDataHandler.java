package org.erlide.tracing.core.utils;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import org.erlide.jinterface.ErlLogger;
import org.erlide.tracing.core.Activator;
import org.erlide.tracing.core.Images;
import org.erlide.tracing.core.TraceType;
import org.erlide.tracing.core.mvc.model.treenodes.FunctionNode;
import org.erlide.tracing.core.mvc.model.treenodes.ITreeNode;
import org.erlide.tracing.core.mvc.model.treenodes.ModuleNode;
import org.erlide.tracing.core.mvc.model.treenodes.TracingResultsNode;
import org.erlide.tracing.core.mvc.model.treenodes.TreeNode;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Handler which reads trace data.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TraceDataHandler {

    // atoms
    private static final String ATOM_ERROR_LOADING = "error_loading";
    private static final Object ATOM_FILE_INFO = "file_info";
    private static final Object ATOM_DROP = "drop";
    private static final String ATOM_STOP_TRACING = "stop_tracing";
    private static final String ATOM_TRACE_TS = "trace_ts";

    // trace tuple fields
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

    // file info tuple fields
    private static final int INDEX_INFO_START_DATE = 1;
    private static final int INDEX_INFO_END_DATE = 2;
    private static final int INDEX_INFO_PATH = 3;
    private static final int INDEX_INFO_COUNT = 4;

    private Date lastTraceDate;
    private String lastProcessDescription;
    private String lastFunctionDescription;
    private final SimpleDateFormat infoDateFormatter = new SimpleDateFormat(
            "dd.MM.yy HH:mm:ss");
    private final SimpleDateFormat nodeDateFormatter = new SimpleDateFormat(
            "HH:mm:ss.SSS dd.MM.yy");

    /**
     * Checks if given message is last one.
     * 
     * @param message
     *            message
     * @return <code>true</code> if it is last message, <code>false</code>
     *         otherwise
     */
    public boolean isTracingFinished(final OtpErlangObject message) {
        if (message instanceof OtpErlangAtom) {
            final OtpErlangAtom atom = (OtpErlangAtom) message;
            if (atom.atomValue().equals(ATOM_STOP_TRACING)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Reads error reason from message.
     * 
     * @param message
     *            message
     * @return error reason or <code>null</code> or if it is not an error
     *         message
     */
    public OtpErlangObject getErrorReson(final OtpErlangObject message) {
        if (message instanceof OtpErlangTuple) {
            final OtpErlangTuple tuple = (OtpErlangTuple) message;
            if (tuple.elementAt(0) instanceof OtpErlangAtom) {
                final OtpErlangAtom atom = (OtpErlangAtom) tuple.elementAt(0);
                if (atom.atomValue().equals(ATOM_ERROR_LOADING)) {
                    return tuple.elementAt(1);
                }
            }
        }
        return null;
    }

    /**
     * Reads data described by given object. It can be trace event or
     * information about file containing tracing results.
     * 
     * @param otpErlangObject
     *            input object
     * 
     * @return tree node that describes data
     */
    public ITreeNode getData(final OtpErlangObject otpErlangObject) {
        try {
            if (otpErlangObject instanceof OtpErlangTuple) {
                final OtpErlangTuple tuple = (OtpErlangTuple) otpErlangObject;

                final String atomValue = ((OtpErlangAtom) tuple.elementAt(0))
                        .atomValue();
                if (atomValue.equals(ATOM_TRACE_TS)) {
                    // trace data: {trace_ts, Data}

                    final OtpErlangAtom traceType = (OtpErlangAtom) tuple
                            .elementAt(INDEX_TRACE_TYPE);
                    lastTraceDate = readDateTuple((OtpErlangTuple) tuple
                            .elementAt(tuple.arity() - 1));

                    switch (TraceType.valueOf(traceType.atomValue()
                            .toUpperCase())) {
                    case CALL:
                        return processCallTrace("Call", tuple);
                    case EXCEPTION_FROM:
                        return processExceptionFrom("Exception", tuple);
                    case EXIT:
                        return processExitTrace("Exit", tuple);
                    case GC_END:
                        return processGcTrace("GC end", Images.GC_END_NODE,
                                tuple);
                    case GC_START:
                        return processGcTrace("GC start", Images.GC_START_NODE,
                                tuple);
                    case GETTING_LINKED:
                        return processLinkTrace("Getting linked",
                                Images.GETTING_LINKED_NODE, tuple);
                    case GETTING_UNLINKED:
                        return processLinkTrace("Getting unlinked",
                                Images.GETTING_UNLINKED_NODE, tuple);
                    case IN:
                        return processInOutTrace("In", Images.IN_NODE, tuple);
                    case LINK:
                        return processLinkTrace("Link", Images.LINK_NODE, tuple);
                    case OUT:
                        return processInOutTrace("Out", Images.OUT_NODE, tuple);
                    case RECEIVE:
                        return processReceiveTrace("Received", tuple);
                    case REGISTER:
                        return processRegisterTrace("Register",
                                Images.REGISTER_NODE, tuple);
                    case RETURN_FROM:
                        return processReturnTrace("Return from",
                                Images.RETURN_FROM_NODE, tuple, true);
                    case RETURN_TO:
                        return processReturnTrace("Return to",
                                Images.RETURN_TO_NODE, tuple, false);
                    case SEND:
                        return processSendTrace("Sent",
                                Images.SENT_MESSAGE_NODE, tuple);
                    case SEND_TO_NON_EXISTING_PROCESS:
                        return processSendTrace("Sent to non existing process",
                                Images.WRONG_MESSAGE_NODE, tuple);
                    case SPAWN:
                        return processSpawnTrace("Spawn", tuple);
                    case UNLINK:
                        return processLinkTrace("Unlink", Images.ULINK_NODE,
                                tuple);
                    case UNREGISTER:
                        return processRegisterTrace("Unregister",
                                Images.UNREGISTER_NODE, tuple);
                    }
                } else if (atomValue.equals(ATOM_FILE_INFO)) {
                    return processFileInfo(tuple);
                } else if (atomValue.equals(ATOM_DROP)) {
                    // drop information: {drop, Long}
                    return processDropTrace(tuple);
                }
            }
        } catch (final Exception e) {
            ErlLogger.error(e);
        }
        return null;
    }

    /**
     * Converts tuple representing date and time (<code>{date(), time()}</code>)
     * to java {@link Date} object.
     * 
     * @param tuple
     *            date tuple
     * @return java date
     * @throws OtpErlangRangeException
     */
    private Date readDateTuple(final OtpErlangTuple tuple)
            throws OtpErlangRangeException {
        final OtpErlangTuple dateTuple = (OtpErlangTuple) tuple.elementAt(0);
        final OtpErlangTuple timeTuple = (OtpErlangTuple) tuple.elementAt(1);

        final int year = ((OtpErlangLong) dateTuple.elementAt(0)).intValue();
        final int month = ((OtpErlangLong) dateTuple.elementAt(1)).intValue() - 1;
        final int day = ((OtpErlangLong) dateTuple.elementAt(2)).intValue();
        final int hour = ((OtpErlangLong) timeTuple.elementAt(0)).intValue();
        final int minute = ((OtpErlangLong) timeTuple.elementAt(1)).intValue();
        final int second = ((OtpErlangLong) timeTuple.elementAt(2)).intValue();

        final Calendar calendar = Calendar.getInstance();
        calendar.set(year, month, day, hour, minute, second);

        return calendar.getTime();
    }

    private String createNodeLabel(final String text) {
        return "[" + nodeDateFormatter.format(lastTraceDate) + "] " + text;
    }

    private String pid2Str(final OtpErlangPid pid) {
        return new StringBuilder().append(pid.id()).append(".")
                .append(pid.serial()).append(".").append(pid.creation())
                .toString();
    }

    // functions creating nodes

    private ITreeNode processDropTrace(final OtpErlangTuple tuple) {
        final OtpErlangLong amount = (OtpErlangLong) tuple.elementAt(1);
        final ITreeNode node = new TreeNode("Dropped traces: "
                + amount.longValue(), Activator.getImage(Images.DROP_NODE));
        return node;
    }

    private ITreeNode processFileInfo(final OtpErlangTuple tuple) {
        TracingResultsNode node = null;
        if (tuple.elementAt(INDEX_INFO_START_DATE) instanceof OtpErlangAtom) {
            // file contains no trace events
            return null;
        }
        try {
            final Date from = readDateTuple((OtpErlangTuple) tuple
                    .elementAt(INDEX_INFO_START_DATE));
            final Date to = readDateTuple((OtpErlangTuple) tuple
                    .elementAt(INDEX_INFO_END_DATE));
            final String path = ((OtpErlangString) tuple
                    .elementAt(INDEX_INFO_PATH)).stringValue();
            final long size = ((OtpErlangLong) tuple
                    .elementAt(INDEX_INFO_COUNT)).longValue();

            node = new TracingResultsNode();
            node.setStartDate(from);
            node.setEndDate(to);
            node.setFileName(path);
            node.setSize(size);

            // node label
            final StringBuilder builder = new StringBuilder();
            builder.append(infoDateFormatter.format(from)).append(" - ")
                    .append(infoDateFormatter.format(to)).append(" (")
                    .append(size).append(" traces): ").append(path);
            node.setLabel(builder.toString());

        } catch (final OtpErlangRangeException e) {
            ErlLogger.error(e);
        }
        return node;
    }

    private ITreeNode createProcessNode(final String label,
            final OtpErlangObject erlangObject) {
        final StringBuilder builder = new StringBuilder();
        ITreeNode functionNode = null;
        ITreeNode nameNode = null;
        ITreeNode pidNode = null;
        ITreeNode processNodeNode = null;

        if (erlangObject instanceof OtpErlangTuple) {
            // tuple: {Pid(), Initial_call()|Registered_name(), Node()} |
            // {Registered_name, Node()}
            final OtpErlangTuple processTuple = (OtpErlangTuple) erlangObject;
            int index = 0;

            // pid
            OtpErlangPid pid = null;
            if (processTuple.arity() == 3) {
                // {Pid(), Initial_call()|Registered_name(), Node()}
                pid = (OtpErlangPid) processTuple.elementAt(INDEX_PROCESS_PID);
                pidNode = new TreeNode("pid: " + pid2Str(pid),
                        Activator.getImage(Images.INFO_NODE));
            } else {
                index = 1;// tuple doesn't contain Pid element
            }

            final OtpErlangObject info = processTuple
                    .elementAt(INDEX_PROCESS_INFO - index);

            // process node
            final OtpErlangObject processNode = processTuple
                    .elementAt(INDEX_PROCESS_NODE - index);
            processNodeNode = new TreeNode("node: " + processNode,
                    Activator.getImage(Images.INFO_NODE));

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
                nameNode = new TreeNode("name: " + info,
                        Activator.getImage(Images.INFO_NODE));
                builder.append(info.toString());
            }
            builder.append(" (").append(processNode).append(")");
        } else if (erlangObject instanceof OtpErlangPid) {
            // Pid
            final OtpErlangPid pid = (OtpErlangPid) erlangObject;
            pidNode = new TreeNode("pid: " + pid2Str(pid),
                    Activator.getImage(Images.INFO_NODE));
            processNodeNode = new TreeNode("node: " + pid.node(),
                    Activator.getImage(Images.INFO_NODE));
            builder.append(pid2Str(pid)).append(" (").append(pid.node())
                    .append(")");
        } else {
            // Atom (registered name)
            nameNode = new TreeNode("name: " + erlangObject,
                    Activator.getImage(Images.INFO_NODE));
            builder.append(erlangObject.toString());
        }

        final ITreeNode node = new TreeNode();
        if (pidNode != null) {
            node.addChildren(pidNode);
        }
        if (nameNode != null) {
            node.addChildren(nameNode);
        }
        if (processNodeNode != null) {
            node.addChildren(processNodeNode);
        }
        if (functionNode != null) {
            node.addChildren(functionNode);
        }

        lastProcessDescription = builder.toString();
        node.setLabel(label + lastProcessDescription);

        return node;
    }

    private ITreeNode createFunctionNode(final String label,
            final OtpErlangObject erlangObject) {
        final ITreeNode node = new TreeNode();
        if (erlangObject instanceof OtpErlangTuple) {
            final OtpErlangTuple functionTuple = (OtpErlangTuple) erlangObject;
            final OtpErlangAtom moduleName = (OtpErlangAtom) functionTuple
                    .elementAt(INDEX_FUNCTION_MODULE);
            final OtpErlangAtom functionName = (OtpErlangAtom) functionTuple
                    .elementAt(INDEX_FUNCTION_NAME);

            // args or arity node
            final TreeNode argsNode = new TreeNode();
            argsNode.setImage(Activator.getImage(Images.INFO_NODE));
            final OtpErlangObject arityOrArgs = functionTuple
                    .elementAt(INDEX_FUNCTION_ARGS);
            int arityValue = -1;
            if (arityOrArgs instanceof OtpErlangList) {
                // last element is a list of arguments
                final OtpErlangList arguments = (OtpErlangList) arityOrArgs;
                final StringBuilder builder = new StringBuilder("arguments: ");
                for (int i = 1; i < arguments.arity(); i++) {
                    builder.append(arguments.elementAt(i)).append(", ");
                }
                arityValue = arguments.arity() - 1;
                argsNode.setLabel(builder.substring(0, builder.length() - 2));
            } else {
                // last element is arity
                try {
                    if (functionTuple.elementAt(INDEX_FUNCTION_ARGS) instanceof OtpErlangInt) {
                        arityValue = ((OtpErlangInt) functionTuple
                                .elementAt(INDEX_FUNCTION_ARGS)).intValue();
                    } else {
                        arityValue = (int) ((OtpErlangLong) functionTuple
                                .elementAt(INDEX_FUNCTION_ARGS)).longValue();
                    }
                    argsNode.setLabel("arity: " + arityValue);
                } catch (final OtpErlangRangeException e) {
                    ErlLogger.error(e);
                }
            }

            // module name node
            final TreeNode moduleNameNode = new ModuleNode(
                    moduleName.atomValue());
            moduleNameNode.setLabel("module: " + moduleName);

            // function name node
            final TreeNode functionNameNode = new FunctionNode(
                    moduleName.atomValue(), functionName.atomValue(),
                    arityValue);
            functionNameNode.setLabel("function: " + functionName);

            node.addChildren(moduleNameNode, functionNameNode, argsNode);
            lastFunctionDescription = label + moduleName + ":" + functionName
                    + "/" + arityValue;

        } else {
            lastFunctionDescription = "unknown";
        }
        node.setLabel(lastFunctionDescription);
        return node;
    }

    private ITreeNode createMessageNode(final OtpErlangObject message) {
        final ITreeNode node = new TreeNode(message.toString(),
                Activator.getImage(Images.MESSAGE_NODE));
        return node;
    }

    // functions processing different trace types

    private ITreeNode processGcTrace(final String label, final Images image,
            final OtpErlangTuple tuple) {
        final StringBuilder labelBuilder = new StringBuilder(label)
                .append(": ");

        final ITreeNode processNode = createProcessNode("process: ",
                tuple.elementAt(INDEX_PROCESS));
        processNode.setImage(Activator.getImage(Images.PROCESS_NODE));
        labelBuilder.append(lastProcessDescription);

        final ITreeNode node = new TreeNode(
                createNodeLabel(labelBuilder.toString()),
                Activator.getImage(image));
        node.addChildren(processNode);

        final OtpErlangList list = (OtpErlangList) tuple.elementAt(INDEX_INFO);
        for (final OtpErlangObject otpErlangObject : list) {
            final OtpErlangTuple infoTuple = (OtpErlangTuple) otpErlangObject;
            final OtpErlangObject key = infoTuple.elementAt(0);
            final OtpErlangObject value = infoTuple.elementAt(1);
            final TreeNode treeNode = new TreeNode(key.toString() + ": "
                    + value.toString());
            treeNode.setImage(Activator.getImage(Images.INFO_NODE));
            node.addChildren(treeNode);
        }
        return node;
    }

    private ITreeNode processSpawnTrace(final String label,
            final OtpErlangTuple tuple) {
        final StringBuilder labelBuilder = new StringBuilder(label)
                .append(": ");

        final ITreeNode processNode = createProcessNode("process: ",
                tuple.elementAt(INDEX_PROCESS));
        processNode.setImage(Activator.getImage(Images.PROCESS_NODE));
        labelBuilder.append(lastProcessDescription).append("->");

        final ITreeNode processNode2 = createProcessNode("new process: ",
                tuple.elementAt(INDEX_PROCESS2));
        processNode2.setImage(Activator.getImage(Images.NEW_PROCESS_NODE));
        labelBuilder.append(lastProcessDescription);

        final ITreeNode functionNode = createFunctionNode("function: ",
                tuple.elementAt(INDEX_SPAWN_FUNCTION));
        functionNode.setImage(Activator.getImage(Images.FUNCTION_NODE));

        final ITreeNode node = new TreeNode(
                createNodeLabel(labelBuilder.toString()),
                Activator.getImage(Images.SPAWN_NODE));
        node.addChildren(processNode, processNode2, functionNode);
        return node;
    }

    private ITreeNode processExceptionFrom(final String label,
            final OtpErlangTuple tuple) {
        final StringBuilder labelBuilder = new StringBuilder(label)
                .append(": ");

        final ITreeNode processNode = createProcessNode("process: ",
                tuple.elementAt(INDEX_PROCESS));
        processNode.setImage(Activator.getImage(Images.PROCESS_NODE));

        final OtpErlangTuple exceptionTuple = (OtpErlangTuple) tuple
                .elementAt(INDEX_EXCEPTION);
        final OtpErlangObject exceptionClass = exceptionTuple
                .elementAt(INDEX_EXCEPTION_CLASS);
        final OtpErlangObject exceptionValue = exceptionTuple
                .elementAt(INDEX_EXCEPTION_VALUE);
        labelBuilder.append(exceptionClass.toString());

        final ITreeNode exceptionClassNode = new TreeNode(
                exceptionClass.toString(), Activator.getImage(Images.INFO_NODE));
        exceptionClassNode.addChildren(new TreeNode(exceptionValue.toString()));

        final ITreeNode node = processReturnTrace(labelBuilder.toString(),
                Images.EXCEPTION_NODE, tuple, false);
        node.addChildren(processNode, exceptionClassNode);
        return node;
    }

    private ITreeNode processReturnTrace(final String label,
            final Images image, final OtpErlangTuple tuple,
            final boolean showRetValue) {
        final StringBuilder labelBuilder = new StringBuilder(label)
                .append(": ");

        final ITreeNode processNode = createProcessNode("process: ",
                tuple.elementAt(INDEX_PROCESS));
        processNode.setImage(Activator.getImage(Images.PROCESS_NODE));

        final ITreeNode functionNode = createFunctionNode("function: ",
                tuple.elementAt(INDEX_FUNCTION));
        functionNode.setImage(Activator.getImage(Images.FUNCTION_NODE));
        labelBuilder.append(lastFunctionDescription);

        final ITreeNode node = new TreeNode();
        node.addChildren(processNode, functionNode);

        if (showRetValue) {
            final ITreeNode returnValueNode = new TreeNode("return value: "
                    + tuple.elementAt(INDEX_RETURN_VALUE));
            returnValueNode.setImage(Activator.getImage(Images.INFO_NODE));
            labelBuilder.append("->").append(
                    tuple.elementAt(INDEX_RETURN_VALUE));
            node.addChildren(returnValueNode);
        }

        node.setLabel(createNodeLabel(labelBuilder.toString()));
        node.setImage(Activator.getImage(image));
        return node;
    }

    private ITreeNode processInOutTrace(final String label, final Images image,
            final OtpErlangTuple tuple) {
        final StringBuilder labelBuilder = new StringBuilder(label)
                .append(": ");

        final ITreeNode processNode = createProcessNode("process: ",
                tuple.elementAt(INDEX_PROCESS));
        processNode.setImage(Activator.getImage(Images.PROCESS_NODE));
        labelBuilder.append(lastProcessDescription);

        final ITreeNode functionNode = createFunctionNode("function: ",
                tuple.elementAt(INDEX_FUNCTION));
        functionNode.setImage(Activator.getImage(Images.FUNCTION_NODE));

        final ITreeNode node = new TreeNode(
                createNodeLabel(labelBuilder.toString()),
                Activator.getImage(image));
        node.addChildren(processNode, functionNode);
        return node;
    }

    private ITreeNode processRegisterTrace(final String label,
            final Images image, final OtpErlangTuple tuple) {
        final StringBuilder labelBuilder = new StringBuilder(label)
                .append(": ");

        final ITreeNode process = createProcessNode("process: ",
                tuple.elementAt(INDEX_PROCESS));
        process.setImage(Activator.getImage(Images.REGISTER_NODE));
        labelBuilder.append(lastProcessDescription).append("->");

        final TreeNode regName = new TreeNode("name: "
                + tuple.elementAt(INDEX_REGNAME).toString(),
                Activator.getImage(Images.INFO_NODE));
        labelBuilder.append(regName);

        final ITreeNode node = new TreeNode(
                createNodeLabel(labelBuilder.toString()),
                Activator.getImage(image));
        node.addChildren(process, regName);
        return node;
    }

    private ITreeNode processLinkTrace(final String label, final Images image,
            final OtpErlangTuple tuple) {
        final StringBuilder labelBuilder = new StringBuilder(label)
                .append(": ");

        final ITreeNode process1Node = createProcessNode("process 1: ",
                tuple.elementAt(INDEX_PROCESS));
        process1Node.setImage(Activator.getImage(Images.PROCESS_NODE));
        labelBuilder.append(lastProcessDescription).append("->");

        final ITreeNode process2Node = createProcessNode("process 2: ",
                tuple.elementAt(INDEX_PROCESS2));
        process2Node.setImage(Activator.getImage(Images.PROCESS_NODE));
        labelBuilder.append(lastProcessDescription);

        final TreeNode node = new TreeNode(
                createNodeLabel(labelBuilder.toString()));
        node.setImage(Activator.getImage(image));
        node.addChildren(process1Node, process2Node);
        return node;
    }

    private ITreeNode processExitTrace(final String label,
            final OtpErlangTuple tuple) {
        final StringBuilder labelBuilder = new StringBuilder(label)
                .append(": ");

        final ITreeNode processNode = createProcessNode("process: ",
                tuple.elementAt(INDEX_PROCESS));
        processNode.setImage(Activator.getImage(Images.PROCESS_NODE));
        labelBuilder.append(lastProcessDescription);

        final ITreeNode reasonNode = new TreeNode("reason: "
                + tuple.elementAt(INDEX_REASON).toString(),
                Activator.getImage(Images.INFO_NODE));

        final TreeNode node = new TreeNode(
                createNodeLabel(labelBuilder.toString()));
        node.setImage(Activator.getDefault().getImageRegistry()
                .get(Images.EXIT_NODE.toString()));
        node.addChildren(processNode, reasonNode);
        return node;
    }

    private ITreeNode processReceiveTrace(final String label,
            final OtpErlangTuple tuple) {
        final StringBuilder labelBuilder = new StringBuilder(label)
                .append(": ");

        final ITreeNode processNode = createProcessNode("receiver: ",
                tuple.elementAt(INDEX_PROCESS));
        processNode.setImage(Activator.getImage(Images.RECEIVER_NODE));
        labelBuilder.append(lastProcessDescription);

        final ITreeNode messageNode = createMessageNode(tuple
                .elementAt(INDEX_MESSAGE));

        final TreeNode node = new TreeNode(
                createNodeLabel(labelBuilder.toString()));
        node.setImage(Activator.getDefault().getImageRegistry()
                .get(Images.RECEIVED_MESSAGE_NODE.toString()));
        node.addChildren(processNode, messageNode);
        return node;
    }

    private ITreeNode processSendTrace(final String label, final Images image,
            final OtpErlangTuple tuple) {
        final StringBuilder labelBuilder = new StringBuilder(label)
                .append(": ");

        final ITreeNode senderNode = createProcessNode("sender: ",
                tuple.elementAt(INDEX_PROCESS));
        senderNode.setImage(Activator.getImage(Images.SENDER_NODE));
        labelBuilder.append(lastProcessDescription).append("->");

        final ITreeNode receiverNode = createProcessNode("receiver: ",
                tuple.elementAt(INDEX_TO));
        receiverNode.setImage(Activator.getImage(Images.RECEIVER_NODE));
        labelBuilder.append(lastProcessDescription);

        final ITreeNode messageNode = createMessageNode(tuple
                .elementAt(INDEX_MESSAGE));

        final TreeNode node = new TreeNode(
                createNodeLabel(labelBuilder.toString()));
        node.setImage(Activator.getImage(image));
        node.addChildren(senderNode, receiverNode, messageNode);
        return node;
    }

    private ITreeNode processCallTrace(final String label,
            final OtpErlangTuple tuple) {
        final ITreeNode processNode = createProcessNode("process: ",
                tuple.elementAt(INDEX_PROCESS));
        processNode.setImage(Activator.getImage(Images.PROCESS_NODE));

        final ITreeNode node = createFunctionNode(
                createNodeLabel(label + ": "), tuple.elementAt(INDEX_FUNCTION));
        node.setImage(Activator.getImage(Images.CALL_NODE));
        node.addChildren(processNode);
        return node;
    }
}
