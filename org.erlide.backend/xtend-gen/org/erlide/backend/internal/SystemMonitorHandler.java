package org.erlide.backend.internal;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.eventbus.Subscribe;
import org.erlide.runtime.api.ErlSystemStatus;
import org.erlide.runtime.api.IErlRuntime;
import org.erlide.runtime.events.ErlEvent;
import org.erlide.runtime.events.ErlangEventHandler;

@SuppressWarnings("all")
public class SystemMonitorHandler extends ErlangEventHandler {
  public SystemMonitorHandler(final String backendName) {
    super("system_status", backendName);
  }
  
  @Subscribe
  public void handleEvent(final ErlEvent event) {
    String _topic = event.getTopic();
    String _topic_1 = this.getTopic();
    boolean _equals = _topic.equals(_topic_1);
    boolean _not = (!_equals);
    if (_not) {
      return;
    }
    OtpErlangObject _event = event.getEvent();
    final OtpErlangTuple t = ((OtpErlangTuple) _event);
    IErlRuntime _runtime = event.getRuntime();
    ErlSystemStatus _erlSystemStatus = new ErlSystemStatus(t);
    _runtime.setSystemStatus(_erlSystemStatus);
  }
}
