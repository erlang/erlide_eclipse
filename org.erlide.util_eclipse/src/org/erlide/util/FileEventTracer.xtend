package org.erlide.util

import java.io.BufferedWriter
import java.io.FileWriter
import java.io.IOException
import java.io.PrintWriter
import java.text.SimpleDateFormat
import java.util.Date
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.Path
import java.io.File

class FileEventTracer extends ErlideEventTracerHandler {
    long sessionStartTime
    val IPath storagePath
    PrintWriter file
    val SimpleDateFormat formatter = new SimpleDateFormat("yyyyMMdd-HHmmss-SSS")

    new(String path) {
        storagePath = new Path(path).append(machine).append(user).append(Integer::toHexString(workspace.hashCode))
        new File(storagePath.toPortableString).mkdirs
    }

    def dispatch handle(ErlideSessionEvent event) {
        sessionStartTime = event.timestamp
        val Date date = new Date(sessionStartTime)
        val String sdate = formatter.format(date)
        val String name = storagePath.append(sdate + ".log").toPortableString
        try {
            file = new PrintWriter(new BufferedWriter(new FileWriter(name, false)))
        } catch (IOException e) {
            ErlLogger::warn("Could not create event trace log file: %s", name)
            file = null
        }

            event.print(file)
    }

    def dispatch handle(ErlideEvent event) {
            event.print(file)
    }

    override dispose() {
        if (file !== null) {
            try {
                file.flush
                file.close
            } catch (IOException e) {
                e.printStackTrace

                // nothing to do
                }
            }
        }
    }
