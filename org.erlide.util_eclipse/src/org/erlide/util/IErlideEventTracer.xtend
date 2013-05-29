package org.erlide.util

interface IErlideEventTracer {
    def void traceSession()

    def void traceReset()

    def void traceCrash(String backend)

    def void traceStatus(Object status)

    def void traceOperation(String operation, long duration)


}
