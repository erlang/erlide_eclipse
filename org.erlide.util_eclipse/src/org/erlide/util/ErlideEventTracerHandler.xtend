package org.erlide.util

import java.net.Inet4Address
import org.eclipse.core.resources.ResourcesPlugin
import org.osgi.service.event.EventHandler
import org.osgi.service.event.Event

abstract class ErlideEventTracerHandler implements EventHandler, IDisposable {
    protected String user = System::getProperty("user.name")
    protected String machine = Inet4Address::localHost.canonicalHostName
    protected String workspace = ResourcesPlugin::workspace.root.location.toPortableString

    override void handleEvent(Event event) {
        val ErlideEvent myEvent = event.getProperty("event") as ErlideEvent
        handle(myEvent)
    }

    def abstract void handle(ErlideEvent event)

}
