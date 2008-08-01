package erlang;

import java.util.Iterator;
import java.util.Map;

import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.IdeBackend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideIndent {

	private static OtpErlangList fixIndentPrefs(final Map<String, String> m) {
		final OtpErlangObject[] o = new OtpErlangObject[m.size()];
		final Iterator<Map.Entry<String, String>> im = m.entrySet().iterator();
		for (int i = 0; i < o.length; ++i) {
			final Map.Entry<String, String> e = im.next();
			final OtpErlangAtom a = new OtpErlangAtom(e.getKey());
			final int n = Integer.parseInt(e.getValue());
			final OtpErlangLong l = new OtpErlangLong(n);
			final OtpErlangTuple t = new OtpErlangTuple(new OtpErlangObject[] {
					a, l });
			o[i] = t;
		}
		return new OtpErlangList(o);
	}

	@SuppressWarnings("boxing")
	public static IndentResult indentLine(final IdeBackend b,
			final String oldLine, final String txt, String insertedText,
			final int tabw, final Map<String, String> prefs)
			throws ErlangRpcException, BackendException, RpcException,
			OtpErlangRangeException {
		final OtpErlangObject o = b.rpcx("erlide_indent", "indent_line",
				"sssix", txt, oldLine, insertedText, tabw,
				fixIndentPrefs(prefs));
		return new IndentResult(o);
	}

	@SuppressWarnings("boxing")
	public static OtpErlangObject indentLines(final IdeBackend b,
			final int offset, final String text, final int tabw,
			final Map<String, String> prefs) throws RpcException,
			BackendException {
		final OtpErlangObject o = b.rpcx("erlide_indent", "indent_lines",
				"siix", text, offset, tabw, fixIndentPrefs(prefs));
		return o;
	}
}
