package erlang;

import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

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

	public static int[] indentLine(IBackend b, String line, String txt,
			int lineNumber, int tabw, int[] prefs) throws ErlangRpcException,
			BackendException, RpcException, OtpErlangRangeException {
		final OtpErlangObject o = b.rpcx("erlide_indent", "indent_line",
				"ssiili", txt, line, lineNumber, tabw, prefs);

		if (o instanceof OtpErlangTuple) {
			final OtpErlangTuple t = (OtpErlangTuple) o;
			final OtpErlangLong l0 = (OtpErlangLong) t.elementAt(0);
			final OtpErlangLong l1 = (OtpErlangLong) t.elementAt(1);
			return new int[] { l0.intValue(), l1.intValue() };
		} else {
			return new int[] { 0, 0 };
		}
	}
}
