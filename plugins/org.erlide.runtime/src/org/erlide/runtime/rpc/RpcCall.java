package org.erlide.runtime.rpc;

import java.util.concurrent.Callable;

import com.ericsson.otp.erlang.OtpErlangObject;

@SuppressWarnings("all")
public class RpcCall implements Callable<OtpErlangObject> {
    private final IOtpRpc rpc;

    private final String module;

    private final String function;

    private final String signature;

    private final Object[] args;

    private long timeout;

    private OtpErlangObject groupLeader = null;

    private IRpcCallback callback = null;

    public RpcCall setTimeout(final long timeout) {
        this.timeout = timeout;
        return this;
    }

    public RpcCall setGroupLeader(final OtpErlangObject groupLeader) {
        this.groupLeader = groupLeader;
        return this;
    }

    public RpcCall setCallback(final IRpcCallback callback) {
        this.callback = callback;
        return this;
    }

    @Override
    public OtpErlangObject call() throws Exception {
        return rpc.call(module, function, signature, args);
    }

    public RpcCall(final IOtpRpc rpc, final String module, final String function,
            final String signature, final Object[] args) {
        super();
        this.rpc = rpc;
        this.module = module;
        this.function = function;
        this.signature = signature;
        this.args = args;
    }

    public IOtpRpc getRpc() {
        return rpc;
    }

    public String getModule() {
        return module;
    }

    public String getFunction() {
        return function;
    }

    public String getSignature() {
        return signature;
    }

    public Object[] getArgs() {
        return args;
    }

    public long getTimeout() {
        return timeout;
    }

    public OtpErlangObject getGroupLeader() {
        return groupLeader;
    }

    public IRpcCallback getCallback() {
        return callback;
    }
}
