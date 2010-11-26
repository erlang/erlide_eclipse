package erlang;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideIndent {

    private static List<OtpErlangTuple> fixIndentPrefs(
            final Map<String, String> m) {
        final List<OtpErlangTuple> result = new ArrayList<OtpErlangTuple>(
                m.size());
        for (final Map.Entry<String, String> e : m.entrySet()) {
            final OtpErlangAtom a = new OtpErlangAtom(e.getKey());
            final String s = e.getValue();
            int n;
            if (s.equals("false")) {
                n = 0;
            } else if (s.equals("true")) {
                n = 1;
            } else {
                n = Integer.parseInt(s);
            }
            final OtpErlangLong l = new OtpErlangLong(n);
            final OtpErlangTuple t = new OtpErlangTuple(new OtpErlangObject[] {
                    a, l });
            result.add(t);
        }
        return result;
    }

    @SuppressWarnings("boxing")
    public static IndentResult indentLine(final Backend b,
            final String oldLine, final String txt, final String insertedText,
            final int tabw, final boolean useTabs,
            final Map<String, String> prefs) throws BackendException,
            OtpErlangRangeException {
        // ErlLogger.debug("indentLine '%s'", txt);
        final OtpErlangObject o = b.call("erlide_indent", "indent_line",
                "sssiox", txt, oldLine, insertedText, tabw, useTabs,
                fixIndentPrefs(prefs));
        return new IndentResult(o);
    }

    @SuppressWarnings("boxing")
    public static OtpErlangObject indentLines(final Backend b,
            final int offset, final int length, final String text,
            final int tabw, final boolean useTabs,
            final Map<String, String> prefs) throws BackendException {
        final OtpErlangObject o = b.call(20000, "erlide_indent",
                "indent_lines", "siiiolx", text, offset, length, tabw, useTabs,
                fixIndentPrefs(prefs));
        return o;
    }

    public static OtpErlangObject templateIndentLines(final Backend b,
            final String prefix, final String text, final int tabw,
            final boolean useTabs, final Map<String, String> prefs)
            throws BackendException {
        final OtpErlangObject o = b.call(20000, "erlide_indent",
                "template_indent_lines", "ssiolx", prefix, text, tabw, useTabs,
                fixIndentPrefs(prefs));
        return o;
    }

    @SuppressWarnings("boxing")
    public static OtpErlangObject call(final Backend b, final String module,
            final String fun, final int offset, final int length,
            final String text) {
        try {
            final OtpErlangObject r1 = b.call(module, fun, "sii", text, offset,
                    length);
            return r1;
        } catch (final BackendException e) {
            return new OtpErlangString("");
        }
    }

}
