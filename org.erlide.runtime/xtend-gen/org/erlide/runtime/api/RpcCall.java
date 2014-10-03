package org.erlide.runtime.api;

import com.ericsson.otp.erlang.OtpErlangObject;
import java.util.concurrent.Callable;
import org.eclipse.xtend.lib.annotations.Accessors;
import org.eclipse.xtend.lib.annotations.FinalFieldsConstructor;
import org.eclipse.xtext.xbase.lib.Pure;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.rpc.IRpcCallback;

@Accessors
@FinalFieldsConstructor
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
  
  public OtpErlangObject call() throws Exception {
    return this.rpc.call(this.module, this.function, this.signature, this.args);
  }
  
  public RpcCall(final IOtpRpc rpc, final String module, final String function, final String signature, final Object[] args) {
    super();
    this.rpc = rpc;
    this.module = module;
    this.function = function;
    this.signature = signature;
    this.args = args;
  }
  
  @Pure
  public IOtpRpc getRpc() {
    return this.rpc;
  }
  
  @Pure
  public String getModule() {
    return this.module;
  }
  
  @Pure
  public String getFunction() {
    return this.function;
  }
  
  @Pure
  public String getSignature() {
    return this.signature;
  }
  
  @Pure
  public Object[] getArgs() {
    return this.args;
  }
  
  @Pure
  public long getTimeout() {
    return this.timeout;
  }
  
  @Pure
  public OtpErlangObject getGroupLeader() {
    return this.groupLeader;
  }
  
  @Pure
  public IRpcCallback getCallback() {
    return this.callback;
  }
}
