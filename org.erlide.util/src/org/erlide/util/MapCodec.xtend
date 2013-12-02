package org.erlide.util

import java.util.Map
import com.google.common.base.Splitter

public class MapCodec {

    def static Map<String, String> decode(String string) {
        val result = newHashMap()
        val entries = Splitter.on("!,").split(string)
        for (entry : entries) {
            if (!entry.nullOrEmpty) {
                val split = Splitter.on(":!").split(entry)
                result.put(split.head, split.tail.head)
            }
        }
        return result
    }

    def static String encode(Map<String, String> map) {
        '''«FOR e : map.entrySet»«e.key»:!«e.value»!,«ENDFOR»'''
    }
}
