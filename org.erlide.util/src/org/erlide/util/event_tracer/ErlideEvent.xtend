package org.erlide.util.event_tracer

import java.io.PrintWriter
import java.text.SimpleDateFormat
import java.util.Date

@Data
abstract class ErlideEvent {
    val long timestamp

    def void print(PrintWriter file) {
        if (file !== null)
            file.print(print)
    }

    def abstract String print()
}

class ErlideSessionEvent extends ErlideEvent {
    val static SimpleDateFormat formatter = new SimpleDateFormat("yyyyMMdd-HHmmss-SSS")
    val public int workspace

    new(String aWorkspace) {
        super(System::currentTimeMillis)
        workspace = aWorkspace.hashCode
    }

    override String print() '''
        «timestamp» SESSION «formatter.format(new Date(timestamp))»
    '''

}

class ErlideResetEvent extends ErlideEvent {
    new() {
        super(System::currentTimeMillis)
    }

    override String print() '''
        «timestamp» RESET
        '''
}

class ErlideCrashEvent extends ErlideEvent {
    val String backend

    new(String myBackend) {
        super(System::currentTimeMillis)
        backend = myBackend
    }

    override String print() '''
        «timestamp» CRASH «backend»
        '''
}

class ErlideOperationEvent extends ErlideEvent {
    val String operation
    val long duration

    new(String myOperation, long myDuration) {
        super(System::currentTimeMillis)
        operation = myOperation
        duration = myDuration
    }

    override String print() '''
        «timestamp» OP «duration» «operation»
        '''
}

class ErlideStatusEvent extends ErlideEvent {
    val Object status

    new(Object myStatus) {
        super(System::currentTimeMillis)
        status = myStatus
    }

    override String print() '''
        «timestamp» STATUS «status.toString»
        '''
}
