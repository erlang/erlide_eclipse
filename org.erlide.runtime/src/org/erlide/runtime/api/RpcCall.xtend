package org.erlide.runtime.api

import com.ericsson.otp.erlang.OtpErlangObject
import org.erlide.runtime.rpc.IRpcCallback
import java.util.concurrent.Callable

class RpcCall implements Callable<OtpErlangObject> {
    @Property val String module
    @Property val String function
    @Property val String signature
    @Property val Object[] args
    @Property var long timeout
    @Property var OtpErlangObject groupLeader = null
    @Property var IRpcCallback callback = null

    new(String module, String function, String signature, Object[] args) {
        this._module = module;
        this._function = function;
        this._signature = signature;
        this._args = args;
    }

    def RpcCall setTimeout(long timeout) {
        this._timeout = timeout;
        return this;
    }

    def RpcCall setGroupLeader(OtpErlangObject groupLeader) {
        this._groupLeader = groupLeader;
        return this;
    }

    def RpcCall setCallback(IRpcCallback callback) {
        this._callback = callback;
        return this;
    }

    override call() throws Exception {

    }

}
