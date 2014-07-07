package org.erlide.runtime.api;

import com.ericsson.otp.erlang.OtpErlangObject;
import java.util.concurrent.Callable;
import org.erlide.runtime.rpc.IRpcCallback;

@SuppressWarnings("all")
public class RpcCall implements Callable<OtpErlangObject> {
  private final String _module;
  
  public String getModule() {
    return this._module;
  }
  
  private final String _function;
  
  public String getFunction() {
    return this._function;
  }
  
  private final String _signature;
  
  public String getSignature() {
    return this._signature;
  }
  
  private final Object[] _args;
  
  public Object[] getArgs() {
    return this._args;
  }
  
  private long _timeout;
  
  public long getTimeout() {
    return this._timeout;
  }
  
  private OtpErlangObject _groupLeader = null;
  
  public OtpErlangObject getGroupLeader() {
    return this._groupLeader;
  }
  
  private IRpcCallback _callback = null;
  
  public IRpcCallback getCallback() {
    return this._callback;
  }
  
  public RpcCall(final String module, final String function, final String signature, final Object[] args) {
    this._module = module;
    this._function = function;
    this._signature = signature;
    this._args = args;
  }
  
  public RpcCall setTimeout(final long timeout) {
    this._timeout = timeout;
    return this;
  }
  
  public RpcCall setGroupLeader(final OtpErlangObject groupLeader) {
    this._groupLeader = groupLeader;
    return this;
  }
  
  public RpcCall setCallback(final IRpcCallback callback) {
    this._callback = callback;
    return this;
  }
  
  public OtpErlangObject call() throws Exception {
    return null;
  }
}
