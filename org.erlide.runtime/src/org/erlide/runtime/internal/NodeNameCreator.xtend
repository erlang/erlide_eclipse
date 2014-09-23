package org.erlide.runtime.internal

class NodeNameCreator {
    def static String create() {
        '''jerlide_«timeSuffix»'''
    }

    def static String create(String hostName) {
        '''«org.erlide.runtime.internal.NodeNameCreator.create»@«hostName»'''
    }

    private def static String getTimeSuffix() {
        Long.toHexString(System.currentTimeMillis().bitwiseAnd(0xFFFFFFF#L))
    }

}
