package org.erlide.runtime.internal

import com.ericsson.otp.erlang.OtpNode
import com.ericsson.otp.erlang.OtpNodeStatus
import com.google.common.base.Strings
import org.erlide.util.HostnameChecker

class LocalNodeCreator {

    static def OtpNode createOtpNode(String cookie, boolean longName) {
        val hostName = HostnameChecker.instance.getErlangHostName(longName)
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

    static def void wait_for_epmd(String host) {

        val initializer = new EpmdChecker(host)
    }

}

class ErlideNodeStatus extends OtpNodeStatus {
    val OtpNodeProxy runtime

    new(OtpNodeProxy runtime) {
        this.runtime = runtime
    }

    override void remoteStatus(String node, boolean up, Object info) {
        if (node == runtime.getNodeName() && !up) {
            //runtime.triggerCrashed()
        }
    }
}
