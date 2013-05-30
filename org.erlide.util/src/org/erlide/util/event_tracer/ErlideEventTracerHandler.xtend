package org.erlide.util.event_tracer

import java.net.Inet4Address
import org.erlide.util.IDisposable

abstract class ErlideEventTracerHandler implements IDisposable {
    protected String user = System::getProperty("user.name")
    protected String machine = Inet4Address::localHost.canonicalHostName

    def abstract void handle(ErlideEvent event)

}
