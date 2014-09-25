package org.erlide.runtime.internal

class NodeNameCreator {
    def static String create(String hostName) {
        '''jerlide_«timeSuffix»@«hostName»'''
    }

    private def static String getTimeSuffix() {
        Long.toHexString(System.currentTimeMillis().bitwiseAnd(0xFFFFFFF#L))
    }

}
