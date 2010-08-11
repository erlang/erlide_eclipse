package org.ttb.integration;

import java.util.Date;

import org.erlide.jinterface.util.ErlLogger;
import org.ttb.integration.mvc.model.ITreeNode;
import org.ttb.integration.mvc.model.TreeNode;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRangeException;
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
    private static final String ATOM_STOP_TRACING = "stop_tracing";

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

    private static final int INDEX_TO = 4;
    private static final int INDEX_RETURN_VALUE = 4;
    private static final int INDEX_SPAWN_FUNCTION = 4;

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
    public ITreeNode getData() {
        ITreeNode rootNode = new TreeNode(new Date().toString());
        rootNode.setImage(Activator.getDefault().getImageRegistry().get(Images.ROOT_NODE.toString()));
        while (true) {
            try {
                OtpErlangObject otpErlangObject = otpMbox.receive();
                // TODO remove
                System.out.println("received: " + otpErlangObject);

                if (otpErlangObject instanceof OtpErlangAtom) {
                    OtpErlangAtom atom = (OtpErlangAtom) otpErlangObject;
                    if (atom.atomValue().equals(ATOM_STOP_TRACING)) {
                        return rootNode;
                    }
                } else {
                    OtpErlangTuple tuple = (OtpErlangTuple) otpErlangObject;
                    OtpErlangAtom traceType = (OtpErlangAtom) tuple.elementAt(INDEX_TRACE_TYPE);
                    ITreeNode node = null;

                    switch (TraceType.valueOf(traceType.atomValue().toUpperCase())) {
                    case CALL:
                        node = processCallTrace(tuple);
                        break;
                    case EXCEPTION_FROM:
                        break;
                    case EXIT:
                        node = processExitTrace(tuple);
                        break;
                    case GC_END:
                        // TODO
                        break;
                    case GC_START:
                        // TODO
                        break;
                    case GETTING_LINKED:
                        node = processLinkTrace("Getting linked", Images.GETTING_LINKED_NODE, tuple);
                        break;
                    case GETTING_UNLINKED:
                        node = processLinkTrace("Getting unlinked", Images.GETTING_UNLINKED_NODE, tuple);
                        break;
                    case IN:
                        node = processInOutTrace("In", Images.IN_NODE, tuple);
                        break;
                    case LINK:
                        node = processLinkTrace("Link", Images.LINK_NODE, tuple);
                        break;
                    case OUT:
                        node = processInOutTrace("Out", Images.OUT_NODE, tuple);
                        break;
                    case RECEIVE:
                        node = processReceiveTrace(tuple);
                        break;
                    case REGISTER:
                        node = processRegisterTrace("Register", Images.REGISTER_NODE, tuple);
                        break;
                    case RETURN_FROM:
                        node = processReturnTrace("Return from", Images.RETURN_FROM_NODE, tuple, true);
                        break;
                    case RETURN_TO:
                        node = processReturnTrace("Return from", Images.RETURN_TO_NODE, tuple, false);
                        break;
                    case SEND:
                        node = processSendTrace("Sent message", Images.SENT_MESSAGE_NODE, tuple);
                        break;
                    case SEND_TO_NON_EXISTING_PROCESS:
                        node = processSendTrace("Sent to non existing process", Images.WRONG_MESSAGE_NODE, tuple);
                        break;
                    case SPAWN:
                        node = processSpawnTrace(tuple);
                        break;
                    case ULINK:
                        node = processLinkTrace("Unlink", Images.ULINK_NODE, tuple);
                        break;
                    case UNREGISTER:
                        node = processRegisterTrace("Unregister", Images.UNREGISTER_NODE, tuple);
                        break;
                    }

                    if (node != null) {
                        rootNode.addChildren(node);
                    }
                }
            } catch (Exception e) {
                ErlLogger.error(e);
            }
        }
    }

    // functions creating nodes

    private ITreeNode createProcessNode(String label, OtpErlangObject erlangObject) {
        ITreeNode node = null;
        if (erlangObject instanceof OtpErlangTuple) {
            OtpErlangTuple processTuple = (OtpErlangTuple) erlangObject;
            OtpErlangPid pid = (OtpErlangPid) processTuple.elementAt(INDEX_PROCESS_PID);
            OtpErlangObject info = processTuple.elementAt(INDEX_PROCESS_INFO);
            OtpErlangObject processNode = processTuple.elementAt(INDEX_PROCESS_NODE);

            node = new TreeNode(label + " " + info);
            TreeNode child = new TreeNode("pid: " + pid);
            child.setImage(Activator.getImage(Images.INFO_NODE));
            TreeNode child2 = new TreeNode("node: " + processNode);
            child2.setImage(Activator.getImage(Images.INFO_NODE));
            node.addChildren(child, child2);
        } else {
            node = new TreeNode(label + " " + (erlangObject));
        }
        return node;
    }

    private ITreeNode createFunctionNode(String label, OtpErlangObject erlangObject) {
        ITreeNode node = new TreeNode();
        if (erlangObject instanceof OtpErlangTuple) {
            OtpErlangTuple functionTuple = (OtpErlangTuple) erlangObject;
            OtpErlangAtom moduleName = (OtpErlangAtom) functionTuple.elementAt(INDEX_FUNCTION_MODULE);
            OtpErlangAtom functionName = (OtpErlangAtom) functionTuple.elementAt(INDEX_FUNCTION_NAME);

            // module name node
            TreeNode moduleNameNode = new TreeNode("module: " + moduleName);
            moduleNameNode.setImage(Activator.getImage(Images.INFO_NODE));

            // function name node
            TreeNode functionNameNode = new TreeNode("function: " + functionName);
            functionNameNode.setImage(Activator.getImage(Images.INFO_NODE));

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
                    arityValue = ((OtpErlangInt) functionTuple.elementAt(INDEX_FUNCTION_ARGS)).intValue();
                    argsNode.setLabel("arity: " + arityValue);
                } catch (OtpErlangRangeException e) {
                }
            }
            node.addChildren(moduleNameNode, functionNameNode, argsNode);
            node.setLabel(label + " " + moduleName + ":" + functionName + "/" + arityValue);

        } else {
            node.setLabel(label + " unknown");
        }
        return node;
    }

    private ITreeNode createMessageNode(OtpErlangObject message) {
        ITreeNode node = new TreeNode("message", Activator.getImage(Images.MESSAGE_NODE));

        TreeNode messageNode = new TreeNode(message.toString());
        messageNode.setImage(Activator.getImage(Images.TEXT_NODE));
        node.addChildren(messageNode);
        return node;
    }

    // functions processing different trace types

    private ITreeNode processSpawnTrace(OtpErlangTuple tuple) {
        ITreeNode processNode = createProcessNode("process:", tuple.elementAt(INDEX_PROCESS));
        processNode.setImage(Activator.getImage(Images.PROCESS_NODE));

        ITreeNode processNode2 = createProcessNode("new process:", tuple.elementAt(INDEX_PROCESS2));
        processNode2.setImage(Activator.getImage(Images.NEW_PROCESS_NODE));

        ITreeNode functionNode = createFunctionNode("function:", tuple.elementAt(INDEX_SPAWN_FUNCTION));
        functionNode.setImage(Activator.getImage(Images.FUNCTION_NODE));

        ITreeNode node = new TreeNode("Spawn", Activator.getImage(Images.SPAWN_NODE));
        node.addChildren(processNode, processNode2, functionNode);
        return node;
    }

    private ITreeNode processReturnTrace(String label, Images image, OtpErlangTuple tuple, boolean showRetValue) {
        ITreeNode processNode = createProcessNode("process:", tuple.elementAt(INDEX_PROCESS));
        processNode.setImage(Activator.getImage(Images.PROCESS_NODE));

        ITreeNode functionNode = createFunctionNode("function:", tuple.elementAt(INDEX_FUNCTION));
        functionNode.setImage(Activator.getImage(Images.FUNCTION_NODE));

        ITreeNode node = new TreeNode(label, Activator.getImage(image));
        node.addChildren(processNode, functionNode);

        if (showRetValue) {
            ITreeNode returnValueNode = new TreeNode("return value: " + tuple.elementAt(INDEX_RETURN_VALUE));
            returnValueNode.setImage(Activator.getImage(Images.INFO_NODE));
            node.addChildren(returnValueNode);
        }

        return node;
    }

    private ITreeNode processInOutTrace(String label, Images image, OtpErlangTuple tuple) {
        ITreeNode processNode = createProcessNode("process:", tuple.elementAt(INDEX_PROCESS));
        processNode.setImage(Activator.getImage(Images.PROCESS_NODE));

        ITreeNode functionNode = createFunctionNode("function:", tuple.elementAt(INDEX_FUNCTION));
        functionNode.setImage(Activator.getImage(Images.FUNCTION_NODE));

        ITreeNode node = new TreeNode(label, Activator.getImage(image));
        node.addChildren(processNode, functionNode);
        return node;
    }

    private ITreeNode processRegisterTrace(String label, Images image, OtpErlangTuple tuple) {
        ITreeNode process = createProcessNode("process:", tuple.elementAt(INDEX_PROCESS));
        process.setImage(Activator.getImage(Images.REGISTER_NODE));

        TreeNode regName = new TreeNode("name: " + tuple.elementAt(INDEX_REGNAME).toString(), Activator.getImage(Images.INFO_NODE));
        ITreeNode node = new TreeNode(label, Activator.getImage(image));
        node.addChildren(process, regName);
        return node;
    }

    private ITreeNode processLinkTrace(String label, Images image, OtpErlangTuple tuple) {
        ITreeNode process1Node = createProcessNode("process 1:", tuple.elementAt(INDEX_PROCESS));
        process1Node.setImage(Activator.getImage(Images.PROCESS_NODE));

        ITreeNode process2Node = createProcessNode("process 2:", tuple.elementAt(INDEX_PROCESS2));
        process2Node.setImage(Activator.getImage(Images.PROCESS_NODE));

        TreeNode node = new TreeNode(label);
        node.setImage(Activator.getImage(image));
        node.addChildren(process1Node, process2Node);
        return node;
    }

    private ITreeNode processExitTrace(OtpErlangTuple tuple) {
        ITreeNode processNode = createProcessNode("process:", tuple.elementAt(INDEX_PROCESS));
        processNode.setImage(Activator.getImage(Images.PROCESS_NODE));

        ITreeNode reasonNode = new TreeNode("reason", Activator.getImage(Images.INFO_NODE));
        ITreeNode reasonTextNode = new TreeNode(tuple.elementAt(INDEX_REASON).toString());
        reasonNode.addChildren(reasonTextNode);

        TreeNode node = new TreeNode("Exit");
        node.setImage(Activator.getDefault().getImageRegistry().get(Images.EXIT_NODE.toString()));
        node.addChildren(processNode, reasonNode);
        return node;
    }

    private ITreeNode processReceiveTrace(OtpErlangTuple tuple) {
        ITreeNode processNode = createProcessNode("receiver:", tuple.elementAt(INDEX_PROCESS));
        processNode.setImage(Activator.getImage(Images.RECEIVER_NODE));
        ITreeNode messageNode = createMessageNode(tuple.elementAt(INDEX_MESSAGE));

        TreeNode node = new TreeNode("Received message");
        node.setImage(Activator.getDefault().getImageRegistry().get(Images.RECEIVED_MESSAGE_NODE.toString()));
        node.addChildren(processNode, messageNode);
        return node;
    }

    private ITreeNode processSendTrace(String label, Images image, OtpErlangTuple tuple) {
        ITreeNode senderNode = createProcessNode("sender:", tuple.elementAt(INDEX_PROCESS));
        senderNode.setImage(Activator.getImage(Images.SENDER_NODE));

        ITreeNode receiverNode = createProcessNode("receiver:", tuple.elementAt(INDEX_TO));
        receiverNode.setImage(Activator.getImage(Images.RECEIVER_NODE));

        ITreeNode messageNode = createMessageNode(tuple.elementAt(INDEX_MESSAGE));

        TreeNode node = new TreeNode(label);
        node.setImage(Activator.getImage(image));
        node.addChildren(senderNode, receiverNode, messageNode);
        return node;
    }

    private ITreeNode processCallTrace(OtpErlangTuple tuple) {
        ITreeNode node = createFunctionNode("Call:", tuple.elementAt(INDEX_FUNCTION));
        node.setImage(Activator.getImage(Images.CALL_NODE));
        return node;
    }
}
