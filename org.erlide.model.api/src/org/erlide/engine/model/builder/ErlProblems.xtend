package org.erlide.engine.model.builder

import com.ericsson.otp.erlang.OtpErlangAtom
import com.ericsson.otp.erlang.OtpErlangList
import com.ericsson.otp.erlang.OtpErlangObject
import com.ericsson.otp.erlang.OtpErlangString
import com.ericsson.otp.erlang.OtpErlangTuple
import com.google.common.base.Charsets
import java.util.List
import java.util.Map
import java.util.regex.Pattern
import org.eclipse.xtend.lib.annotations.Data
import org.erlide.util.ErlLogger
import org.erlide.util.Util
import org.erlide.util.erlang.ErlUtils

class ErlProblems {

    val List<ProblemData> data = newArrayList
    val Map<String, ProblemData> tagMap = newHashMap

    private new() {
        load()
    }

    def load() {
        val loader = this.class.getClassLoader()
        val input = loader.getResourceAsStream("org/erlide/engine/model/builder/errors.data")

        try {
            val src = Util.getInputStreamAsString(input, Charsets.ISO_8859_1.name)
            try {
                val source0 = ErlUtils.parse(src)
                val source = source0 as OtpErlangList
                for (OtpErlangObject item0 : source.elements()) {
                    val item = item0 as OtpErlangTuple
                    val tag = (item.elementAt(0) as OtpErlangAtom).atomValue
                    val message = (item.elementAt(1) as OtpErlangString).stringValue.replaceAll("\\\\n", "\n")
                    val myarity = arity(message)
                    val problemData = new ProblemData(tag, message, myarity)
                    data.add(problemData)
                    if (tagMap.containsKey(problemData.tag))
                        throw new IllegalStateException(
                            "duplicate problem tags are not allowed: '" + problemData.tag + "'")
                    tagMap.put(problemData.tag, problemData)
                }
            } catch (Exception e) {
                ErlLogger.debug(e)
                throw e;
            }

        } finally {
            input.close();
        }
    }

    def getData() {
        return data.immutableCopy;
    }

    def static arity(String string) {
        var result = 0;
        var escape = false;
        for (c : string.bytes) {
            if (!escape && c == 126) { // '~'
                result = result + 1
            }
            escape = (c == 92) // '\'
        }
        return result;
    }

    def check() {
        val List<String> names = newArrayList
        for (ProblemData p : data) {
            if (names.contains(p.tag + "/" + p.arity)) {
                println("DOUBLE " + p.tag + "/" + p.arity)
            }
            names.add(p.tag + "/" + p.arity)
        }
    }

    var static ErlProblems instance = null

    def static getInstance() {
        if (instance === null) {
            instance = new ErlProblems()
        }
        return instance
    }

    def static ProblemData parse(String msg) {
        for (p : getInstance().data) {
            if (p.pattern.matcher(msg).matches)
                return p
        }
        return null
    }

    def ProblemData getProblem(String message) {
        for (d : data) {
            val args = d.getMessageArgs(message)
            if (args !== null) {
                return d
            }
        }
        return null
    }
}

@Data
class ProblemData0 {
    String tag
    String message
    int arity
}

class ProblemData extends ProblemData0 {
    val public static TAG = "erlide.tag"
    val public static ARGS = "erlide.args"

    Pattern _pattern

    new(String tag, String message, int arity) {
        super(tag, message, arity)
    }

    def getPattern() {
        if (_pattern === null) {
            val str = quoteRegex(message)
            val key = "@@@"
            _pattern = Pattern.compile(str.replaceAll("\\\\~", key).replaceAll("~", "(.+?)").replaceAll(key, "~"))
        }
        return _pattern
    }

    def setPattern(Pattern p) {
        throw new UnsupportedOperationException("pattern is read-only")
    }

    def getCategory() {
        return tag.split("_").head
    }

    def List<String> getMessageArgs(String msg) {
        val matcher = pattern.matcher(msg)
        if (matcher.matches) {
            val num = matcher.groupCount
            val result = newArrayList
            var i = 1
            while (i <= num) {
                result.add(matcher.group(i))
                i = i + 1
            }
            return result
        }
        return null
    }

    def static String quoteRegex(String string) {
        val esc = "([{^$|)?*+.".bytes
        var result = string
        for (c : esc) {
            val String r = "\\" + c as char
            val String v = "\\\\" + c as char
            result = result.replaceAll(r, v)
        }
        return result
    }

}
