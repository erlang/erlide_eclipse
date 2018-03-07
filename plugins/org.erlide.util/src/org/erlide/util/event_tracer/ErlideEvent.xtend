package org.erlide.util.event_tracer

import java.io.PrintWriter
import java.text.SimpleDateFormat
import java.util.Date
import org.eclipse.xtend.lib.annotations.Data

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
    val SimpleDateFormat formatter = new SimpleDateFormat("yyyyMMdd-HHmmss-SSS")
    val public int workspace

    new(String aWorkspace) {
        super(System.currentTimeMillis)
        workspace = aWorkspace.hashCode
    }

    override String print() '''
        «timestamp» SESSION «formatter.format(new Date(timestamp))»
    '''

}

class ErlideResetEvent extends ErlideEvent {
    new() {
        super(System.currentTimeMillis)
    }

    override String print() '''
        «timestamp» RESET
        '''
}

class ErlideCrashEvent extends ErlideEvent {
    val String backend

    new(String myBackend) {
        super(System.currentTimeMillis)
        backend = myBackend
    }

    override String print() '''
        «timestamp» CRASH «backend»
        '''
}

abstract class ErlideOperationEvent extends ErlideEvent {
    val protected String operation
    val protected String id

    new(String myOperation, String myId) {
        super(System.currentTimeMillis)
        operation = myOperation
        id = myId
    }
}

class ErlideOperationStartEvent extends ErlideOperationEvent {

    new(String myOperation, String myId) {
        super(myOperation, myId)
    }

    override String print() '''
        «timestamp» OP> «id» «operation»
        '''
}

class ErlideOperationEndEvent extends ErlideOperationEvent {

    new(String myOperation, String myId) {
        super(myOperation, myId)
    }

    override String print() '''
        «timestamp» OP< «id» «operation»
        '''
}

class ErlideStatusEvent extends ErlideEvent {
    val Object status

    new(Object myStatus) {
        super(System.currentTimeMillis)
        status = myStatus
    }

    override String print() '''
        «timestamp» STATUS «status.toString»
        '''
}
