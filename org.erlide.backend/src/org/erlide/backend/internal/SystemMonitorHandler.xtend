package org.erlide.backend.internal

import com.ericsson.otp.erlang.OtpErlangTuple
import org.erlide.backend.BackendCore
import org.erlide.backend.events.ErlangEventHandler
import org.osgi.service.event.Event
import org.erlide.runtime.ErlSystemStatus

class SytemMonitorHandler extends ErlangEventHandler {

  new(String backendName) {
    super("system_status", backendName);
  }

  override void handleEvent(Event event) {
    val OtpErlangTuple t = event.getProperty("DATA") as OtpErlangTuple
    val b = BackendCore::backendManager.allBackends.findFirst[name==backendName]
    b.systemStatus = new ErlSystemStatus(t)
  }

}
