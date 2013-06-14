package org.erlide.backend.internal;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.base.Objects;
import com.google.common.eventbus.Subscribe;
import java.util.Collection;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.erlide.backend.BackendCore;
import org.erlide.backend.api.IBackend;
import org.erlide.backend.api.IBackendManager;
import org.erlide.runtime.api.ErlSystemStatus;
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
    IBackendManager _backendManager = BackendCore.getBackendManager();
    Collection<IBackend> _allBackends = _backendManager.getAllBackends();
    final Function1<IBackend,Boolean> _function = new Function1<IBackend,Boolean>() {
        public Boolean apply(final IBackend it) {
          String _name = it.getName();
          String _backendName = SystemMonitorHandler.this.getBackendName();
          boolean _equals = Objects.equal(_name, _backendName);
          return Boolean.valueOf(_equals);
        }
      };
    final IBackend b = IterableExtensions.<IBackend>findFirst(_allBackends, _function);
    ErlSystemStatus _erlSystemStatus = new ErlSystemStatus(t);
    b.setSystemStatus(_erlSystemStatus);
  }
}
