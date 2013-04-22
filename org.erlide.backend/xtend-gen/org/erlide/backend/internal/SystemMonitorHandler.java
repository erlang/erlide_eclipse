package org.erlide.backend.internal;

import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.base.Objects;
import java.util.Collection;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.erlide.backend.BackendCore;
import org.erlide.backend.IBackend;
import org.erlide.backend.IBackendManager;
import org.erlide.backend.events.ErlangEventHandler;
import org.erlide.runtime.ErlSystemStatus;
import org.osgi.service.event.Event;

@SuppressWarnings("all")
public class SystemMonitorHandler extends ErlangEventHandler {
  public SystemMonitorHandler(final String backendName) {
    super("system_status", backendName);
  }
  
  public void handleEvent(final Event event) {
    Object _property = event.getProperty("DATA");
    final OtpErlangTuple t = ((OtpErlangTuple) _property);
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
