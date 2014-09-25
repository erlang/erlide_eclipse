package org.erlide.runtime.internal;

import com.ericsson.otp.erlang.OtpNodeStatus;
import com.google.common.base.Objects;
import org.erlide.runtime.OtpNodeProxy;

@SuppressWarnings("all")
public class ErlideNodeStatus extends OtpNodeStatus {
  private final OtpNodeProxy runtime;
  
  public ErlideNodeStatus(final OtpNodeProxy runtime) {
    this.runtime = runtime;
  }
  
  public void remoteStatus(final String node, final boolean up, final Object info) {
    boolean _and = false;
    String _nodeName = this.runtime.getNodeName();
    boolean _equals = Objects.equal(node, _nodeName);
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
