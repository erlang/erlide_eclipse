package org.erlide.backend.internal

import com.ericsson.otp.erlang.OtpErlangTuple
import com.google.common.eventbus.Subscribe
import org.erlide.runtime.api.ErlSystemStatus
import org.erlide.runtime.events.ErlEvent
import org.erlide.runtime.events.ErlangEventHandler

class SystemMonitorHandler extends ErlangEventHandler {

    new(String backendName) {
        super("system_status", backendName);
    }

    @Subscribe
    def void handleEvent(ErlEvent event) {
        if (!event.topic.equals(topic))
            return

        val OtpErlangTuple t = event.getEvent as OtpErlangTuple
        event.runtime.systemStatus = new ErlSystemStatus(t)
    }

}
