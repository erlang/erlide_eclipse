package org.erlide.runtime.internal;

import com.ericsson.otp.erlang.OtpNodeStatus;
import org.erlide.runtime.internal.ErlRuntime;

@SuppressWarnings("all")
public class ErlideNodeStatus extends OtpNodeStatus {
  private final ErlRuntime runtime;
  
  public ErlideNodeStatus(final ErlRuntime runtime) {
    this.runtime = runtime;
  }
  
  public void remoteStatus(final String node, final boolean up, final Object info) {
    boolean _and = false;
    String _nodeName = this.runtime.getNodeName();
    boolean _equals = node.equals(_nodeName);
    if (!_equals) {
      _and = false;
    } else {
      _and = (!up);
    }
    if (_and) {
      this.runtime.triggerCrashed();
    }
  }
}
