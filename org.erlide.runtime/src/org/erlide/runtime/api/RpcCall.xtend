package org.erlide.runtime.api

import com.ericsson.otp.erlang.OtpErlangObject
import java.util.concurrent.Callable
import org.eclipse.xtend.lib.annotations.Accessors
import org.erlide.runtime.rpc.IRpcCallback
import org.eclipse.xtend.lib.annotations.FinalFieldsConstructor

@Accessors
@FinalFieldsConstructor
class RpcCall implements Callable<OtpErlangObject> {
    val IOtpRpc rpc
    val String module
    val String function
    val String signature
    val Object[] args
    var long timeout
    var OtpErlangObject groupLeader = null
    var IRpcCallback callback = null

    def RpcCall setTimeout(long timeout) {
        this.timeout = timeout;
        return this;
    }

    def RpcCall setGroupLeader(OtpErlangObject groupLeader) {
        this.groupLeader = groupLeader;
        return this;
    }

    def RpcCall setCallback(IRpcCallback callback) {
        this.callback = callback;
        return this;
    }

    override call() throws Exception {
        // TODO
        rpc.call(module, function, signature, args)
    }

}
