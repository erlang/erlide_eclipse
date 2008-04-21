package erlang;

import java.util.Iterator;
import java.util.Map;

import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideIndent {

	// @SuppressWarnings("boxing")
	// public static OtpErlangObject indentNextLine(final IBackend b, String
	// txt,
	// int tabw) throws ErlangRpcException, BackendException, RpcException {
	// final OtpErlangObject r1 = b.rpcx("erlide_indent", "indent_next_line",
	// "si", txt, tabw);
	// return r1;
	// }

	private static OtpErlangList fixIndentPrefs(final Map<String, Integer> m) {
		final OtpErlangObject[] o = new OtpErlangObject[m.size()];
		final Iterator<Map.Entry<String, Integer>> im = m.entrySet().iterator();
		for (int i = 0; i < o.length; ++i) {
			final Map.Entry<String, Integer> e = im.next();
			final OtpErlangAtom a = new OtpErlangAtom(e.getKey());
			final OtpErlangLong l = new OtpErlangLong(e.getValue());
			final OtpErlangTuple t = new OtpErlangTuple(new OtpErlangObject[] {
					a, l });
			o[i] = t;
		}
		return new OtpErlangList(o);
	}

	public static int[] indentLine(final IBackend b, final String oldLine,
			final String txt, String insertedText, final int tabw,
			final Map<String, Integer> prefs) throws ErlangRpcException,
			BackendException, RpcException, OtpErlangRangeException {
		final OtpErlangObject o = b.rpcx("erlide_indent", "indent_line",
				"sssix", txt, oldLine, insertedText, tabw,
				fixIndentPrefs(prefs));

		if (o instanceof OtpErlangTuple) {
			final OtpErlangTuple t = (OtpErlangTuple) o;
			final OtpErlangLong l0 = (OtpErlangLong) t.elementAt(0);
			final OtpErlangLong l1 = (OtpErlangLong) t.elementAt(1);
			return new int[] { l0.intValue(), l1.intValue() };
		} else {
			return new int[] { 0, 0 };
		}
	}

	public static OtpErlangObject indentLines(final IBackend b,
			final int offset, final String text, final int tabw,
			final Map<String, Integer> prefs) throws RpcException,
			BackendException {
		final OtpErlangObject res = b.rpcx("erlide_indent", "indent_lines",
				"siix", text, offset, tabw, fixIndentPrefs(prefs));
		return res;
	}
}
