package org.erlide.runtime.internal

import com.ericsson.otp.erlang.OtpNode
import com.ericsson.otp.erlang.OtpNodeStatus
import com.google.common.base.Strings
import java.io.IOException
import java.net.Socket
import org.erlide.runtime.api.IErlRuntime
import org.erlide.util.ErlLogger
import org.erlide.util.HostnameUtils
import org.fishwife.jrugged.Initializable
import org.fishwife.jrugged.Initializer

class LocalNodeCreator {

    static def OtpNode createOtpNode(String cookie, boolean longName) {
        var OtpNode node;
        val String hostName = HostnameUtils.getErlangHostName(longName);
        if (Strings.isNullOrEmpty(cookie)) {
            node = new OtpNode(NodeNameCreator.create(hostName));
        } else {
            node = new OtpNode(NodeNameCreator.create(hostName), cookie);
        }
        return node;
    }

    static def OtpNode startLocalNode(ErlRuntime runtime, String cookie, boolean hasLongName) {
        wait_for_epmd();
        val OtpNode lNode = createOtpNode(cookie, hasLongName);
        val OtpNodeStatus statusWatcher = new ErlideNodeStatus(runtime);
        lNode.registerStatusHandler(statusWatcher);
        return lNode;
    }

    static def void wait_for_epmd() {
        wait_for_epmd(null);
    }

    static val int EPMD_PORT = Integer.parseInt(System.getProperty("erlide.epmd.port", "4369"));
    public static val long POLL_INTERVAL = 100;

    static def void wait_for_epmd(String host) {

        val client = new Initializable() {

            override afterInit() {
            }

            override configuredRetriesMetOrExceededWithoutSuccess() {
                val String msg = "Couldn't contact epmd - erlang backend is probably not working\n" +
                    "Your host's entry in /etc/hosts is probably wrong (" + host + ")."
                ErlLogger.error(msg)
                throw new RuntimeException(msg)
            }

            override tryInit() throws Exception {
                var Socket s;
                try {
                    s = new Socket(host, EPMD_PORT)
                    s.close()
                } catch (IOException e) {
                }
            }

        }
        val initializer = new Initializer(client)
        initializer.maxRetries = 30
        initializer.retryMillis = POLL_INTERVAL
        initializer.initialize
    }

}

class ErlideNodeStatus extends OtpNodeStatus {
    val ErlRuntime runtime

    new(ErlRuntime runtime) {
        this.runtime = runtime
    }

    override void remoteStatus(String node, boolean up, Object info) {
        if (node.equals(runtime.getNodeName()) && !up) {
            runtime.triggerCrashed();
        }
    }
}
