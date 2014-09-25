package org.erlide.runtime.internal

import com.ericsson.otp.erlang.OtpNode
import com.ericsson.otp.erlang.OtpNodeStatus
import com.google.common.base.Strings
import java.io.IOException
import java.net.Socket
import org.erlide.runtime.OtpNodeProxy
import org.erlide.util.ErlLogger
import org.erlide.util.HostnameUtils
import org.fishwife.jrugged.Initializable
import org.fishwife.jrugged.Initializer

class LocalNodeCreator {

    static def OtpNode createOtpNode(String cookie, boolean longName) {
        val hostName = HostnameUtils.getErlangHostName(longName)
        if (Strings.isNullOrEmpty(cookie)) {
            new OtpNode(NodeNameCreator.create(hostName))
        } else {
            new OtpNode(NodeNameCreator.create(hostName), cookie)
        }
    }

    static def OtpNode startLocalNode(OtpNodeProxy runtime, String cookie, boolean hasLongName) {
        wait_for_epmd()
        val lNode = createOtpNode(cookie, hasLongName)
        val statusWatcher = new ErlideNodeStatus(runtime)
        lNode.registerStatusHandler(statusWatcher)
        return lNode
    }

    static def void wait_for_epmd() {
        wait_for_epmd(null)
    }

    static val EPMD_PORT = Integer.parseInt(System.getProperty("erlide.epmd.port", "4369"))
    public static val long POLL_INTERVAL = 100

    static def void wait_for_epmd(String host) {

        val client = new Initializable() {

            override afterInit() {
            }

            override configuredRetriesMetOrExceededWithoutSuccess() {
                val msg = '''
                    Couldn't contact epmd - erlang backend is probably not working.
                    Your host's entry in /etc/hosts is probably wrong («host»).
                    '''
                ErlLogger.error(msg)
                throw new RuntimeException(msg)
            }

            override tryInit() throws Exception {
                try {
                    val s = new Socket(host, EPMD_PORT)
                    s.close()
                } catch (IOException e) {
                }
            }

        }
        val initializer = new Initializer(client)
        initializer.maxRetries = 30
        initializer.retryMillis = POLL_INTERVAL
        // run synchronously
        initializer.run
    }

}

class ErlideNodeStatus extends OtpNodeStatus {
    val OtpNodeProxy runtime

    new(OtpNodeProxy runtime) {
        this.runtime = runtime
    }

    override void remoteStatus(String node, boolean up, Object info) {
        if (node == runtime.getNodeName() && !up) {
            runtime.triggerCrashed()
        }
    }
}
