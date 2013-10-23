package org.erlide.util.event_tracer

import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import java.io.IOException
import java.io.PrintWriter
import java.net.Inet4Address
import java.text.SimpleDateFormat
import java.util.Date
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.Path
import org.erlide.util.ErlLogger
import org.erlide.util.IDisposable

class ErlideEventTracerHandler implements IDisposable {
    protected String user = System.getProperty("user.name")
    protected String machine = Inet4Address.localHost.canonicalHostName

    val IPath storagePath
    PrintWriter file
    val SimpleDateFormat formatter = new SimpleDateFormat("yyyyMMdd-HHmmss-SSS")

    new(String path) {
        if (path === null) {
            storagePath = null
            return
        }
        storagePath = new Path(path).append(machine).append(user)
        new File(storagePath.toPortableString).mkdirs
    }

    def dispatch handle(ErlideSessionEvent event) {
        if(storagePath === null) return
        val Date date = new Date(event.timestamp)
        val String sdate = formatter.format(date)
        val String name = storagePath.append(Integer.toHexString(event.workspace)).append(sdate + ".log").
            toPortableString
        try {
            file = new PrintWriter(new BufferedWriter(new FileWriter(name, false)))
        } catch (IOException e) {
            ErlLogger.warn("Could not create event trace log file: %s", name)
            file = null
        }

        event.print(file)
    }

    def dispatch handle(ErlideEvent event) {
        event.print(file)
    }

    override dispose() {
        if (file !== null) {
            file.flush
            try {
                file.close
            } catch (IOException e) {
                e.printStackTrace
            }
        }
    }

}
