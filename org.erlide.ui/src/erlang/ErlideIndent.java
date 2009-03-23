package erlang;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.Backend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.runtime.backend.exceptions.NoBackendException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideIndent {

	private static List<OtpErlangTuple> fixIndentPrefs(
			final Map<String, String> m) {
		final List<OtpErlangTuple> result = new ArrayList<OtpErlangTuple>(m
				.size());
		final Iterator<Map.Entry<String, String>> im = m.entrySet().iterator();
		while (im.hasNext()) {
			final Map.Entry<String, String> e = im.next();
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
			final int tabw, final Map<String, String> prefs)
			throws ErlangRpcException, BackendException, RpcException,
			OtpErlangRangeException {
		final OtpErlangObject o = b.rpcx("erlide_indent", "indent_line",
				"sssix", txt, oldLine, insertedText, tabw,
				fixIndentPrefs(prefs));
		return new IndentResult(o);
	}

	@SuppressWarnings("boxing")
	public static OtpErlangObject indentLines(final Backend b,
			final int offset, final int length, final String text,
			final int tabw, final Map<String, String> prefs)
			throws RpcException, BackendException {
		final OtpErlangObject o = b.rpcx("erlide_indent", "indent_lines",
				"siiilx", text, offset, length, tabw, fixIndentPrefs(prefs));
		return o;
	}

	@SuppressWarnings("boxing")
	public static OtpErlangObject call(final Backend b, final String module,
			final String fun, final int offset, final int length,
			final String text) throws BackendException, RpcException {
		try {
			final OtpErlangObject r1 = b.rpcx(module, fun, "sii", text, offset,
					length);
			return r1;
		} catch (final NoBackendException e) {
			return new OtpErlangString("");
		}
	}
}
