package erlang;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class OpenResult {

	private boolean isExternalCall = false;
	private String name;
	private String fun;
	private int arity;
	private String path;
	private boolean isRecord;
	private boolean isMacro;
	private boolean isLocalCall;
	private boolean isInclude;

	public OpenResult(OtpErlangObject res) {
		if (!(res instanceof OtpErlangTuple)) {
			return; // not a call, ignore
		}
		final OtpErlangTuple tres = (OtpErlangTuple) res;
		final String kind = ((OtpErlangAtom) tres.elementAt(0)).atomValue();
		try {
			if (kind.equals("external")) {
				isExternalCall = true;
				name = ((OtpErlangAtom) tres.elementAt(1)).atomValue();
				fun = ((OtpErlangAtom) tres.elementAt(2)).atomValue();
				arity = ((OtpErlangLong) tres.elementAt(3)).intValue();
				path = null;
				if (tres.arity() > 4
						&& tres.elementAt(4) instanceof OtpErlangString) {
					path = ((OtpErlangString) tres.elementAt(4)).stringValue();
				}
			} else if (kind.equals("include")) {
				isInclude = true;
				final OtpErlangString s = (OtpErlangString) tres.elementAt(1);
				name = s.stringValue();
			} else if (kind.equals("local")) { // local call
				isLocalCall = true;
				fun = ((OtpErlangAtom) tres.elementAt(1)).atomValue();
				arity = ((OtpErlangLong) tres.elementAt(2)).intValue();
				// } else if (external.equals("variable")) {
				// final OtpErlangTuple mf = (OtpErlangTuple) tres.elementAt(1);
				// final OtpErlangAtom var = (OtpErlangAtom) mf.elementAt(0);
			} else if (kind.equals("record") || kind.equals("macro")) {
				isMacro = kind.equals("macro");
				isRecord = kind.equals("record");
				name = ((OtpErlangAtom) tres.elementAt(1)).atomValue();
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public boolean isExternalCall() {
		return isExternalCall;
	}

	public String getName() {
		return name;
	}

	public String getFun() {
		return fun;
	}

	public int getArity() {
		return arity;
	}

	public String getPath() {
		return path;
	}

	public boolean isRecord() {
		return isRecord;
	}

	public boolean isMacro() {
		return isMacro;
	}

	public boolean isLocalCall() {
		return isLocalCall;
	}

	public boolean isInclude() {
		return isInclude;
	}

}
