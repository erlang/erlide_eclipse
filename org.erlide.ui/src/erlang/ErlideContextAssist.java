package erlang;

import java.util.Collection;
import java.util.SortedSet;
import java.util.TreeSet;

import org.erlide.backend.Backend;
import org.erlide.backend.BackendException;
import org.erlide.core.util.Util;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideContextAssist {

	public static Collection<String> getVariables(final Backend b,
			final String src, final String prefix) {
		final SortedSet<String> result = new TreeSet<String>();
		try {
			final OtpErlangObject res = b.call("erlide_content_assist",
					"get_variables", "ss", src, prefix);
			if (Util.isOk(res)) {
				final OtpErlangTuple t = (OtpErlangTuple) res;
				final OtpErlangList l = (OtpErlangList) t.elementAt(1);
				for (final OtpErlangObject i : l.elements()) {
					result.add(Util.stringValue(i));
				}
			}
		} catch (final BackendException e) {
			e.printStackTrace();
		}
		return result;
	}

	public static boolean checkRecordCompletion(final Backend b,
			final String substring) {
		try {
			final OtpErlangObject res = b.call("erlide_content_assist",
					"check_record", "s", substring);
			if (Util.isOk(res)) {
				final OtpErlangTuple t = (OtpErlangTuple) res;
				final OtpErlangAtom a = (OtpErlangAtom) t.elementAt(1);
				return a.booleanValue();
			}
		} catch (final BackendException e) {
			e.printStackTrace();
		}
		return false;
	}

	public static OtpErlangList getFunctionHead(final Backend b,
			final String name, final int arity) {
		try {
			final OtpErlangObject res = b.call("erlide_content_assist",
					"get_function_head", "ai", name, arity);
			if (res instanceof OtpErlangList) {
				return (OtpErlangList) res;
			}
		} catch (final BackendException e) {
			e.printStackTrace();
		}
		return null;
	}

}
