package org.erlide.runtime.internal

import com.ericsson.otp.erlang.OtpNode
import com.ericsson.otp.erlang.OtpNodeStatus
import com.google.common.base.Strings
import java.io.IOException
import java.net.Socket
import org.erlide.runtime.api.IErlRuntime
import org.erlide.util.ErlLogger
import org.erlide.util.HostnameUtils

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

        // If anyone has a better solution for waiting for epmd to be up, please
        // let me know
        var int tries = 30;
        var boolean ok = false;
        do {
            var Socket s;
            try {
                s = new Socket(host, EPMD_PORT);
                s.close();
                ok = true;
            } catch (IOException e) {
            }
            try {
                Thread.sleep(POLL_INTERVAL);
            } catch (InterruptedException e1) {
            }
            tries--;
        } while (!ok && tries > 0);
        if (!ok) {
            val String msg = "Couldn't contact epmd - erlang backend is probably not working\n" +
                "Your host's entry in /etc/hosts is probably wrong (" + host + ").";
            ErlLogger.error(msg);
            throw new RuntimeException(msg);
        }
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
