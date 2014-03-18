package org.erlide.engine.model.builder

import com.ericsson.otp.erlang.OtpErlangAtom
import com.ericsson.otp.erlang.OtpErlangList
import com.ericsson.otp.erlang.OtpErlangObject
import com.ericsson.otp.erlang.OtpErlangString
import com.ericsson.otp.erlang.OtpErlangTuple
import java.util.List
import java.util.Scanner
import java.util.regex.Pattern
import org.erlide.util.ErlLogger
import org.erlide.util.erlang.ErlUtils
import org.erlide.util.erlang.OtpPatternVariable

class ErlProblems {

    val List<ProblemData> data = newArrayList()

    private new() {
        load()
    }

    def load() {
        val loader = this.class.getClassLoader()
        val input = loader.getResourceAsStream("org/erlide/engine/model/builder/errors.data")

        try {
            val s = new Scanner(input).useDelimiter("\\A")
            val src = if (s.hasNext()) s.next() else ""
            try {
                val source0 = ErlUtils.parse(src)
                val source = source0 as OtpErlangList
                for (OtpErlangObject category0 : source.elements) {
                    val category = category0 as OtpErlangTuple
                    val categoryName = (category.elementAt(0) as OtpErlangAtom).atomValue
                    for (OtpErlangObject item0 : (category.elementAt(1) as OtpErlangList).elements()) {
                        val item = item0 as OtpErlangTuple
                        val tag = (item.elementAt(0) as OtpErlangAtom).atomValue
                        val message = (item.elementAt(1) as OtpErlangString).stringValue.replaceAll("\\\\n", "\n")
                        //message = quoteRegex(message) //.replace("~", "(.+?)")
                        data.add(new ProblemData(categoryName, tag, message, arity(message)))
                    }
                }
            } catch (Exception e) {
                ErlLogger.debug(e)
                throw e;
            }

        } finally {
            input.close();
        }
    }

    def arity(String string) {
        var result = 0;
        for (c : string.bytes) {
            if (c == 126) // '~'
                result = result + 1
        }
        return result;
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

    def check() {
        val List<String> names = newArrayList
        for (ProblemData p : data) {
            if (names.contains(p.tag + "/" + p.arity)) {
                println("DOUBLE " + p.tag + "/" + p.arity)
            }
            names.add(p.tag + "/" + p.arity)
        }
    }

    val public static ErlProblems instance = new ErlProblems()

    def static ProblemData parse(String msg) {
        return null
    }

}

@Data
class ProblemData {
    String category
    String tag
    String message
    int arity
}
