package erlang;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class IndentResult {
	public int indentWith;
	public int removeNext;
	public boolean addNewLine;

	public IndentResult(int indentWith, int removeNext, boolean addNewLine) {
		super();
		this.indentWith = indentWith;
		this.removeNext = removeNext;
		this.addNewLine = addNewLine;
	}

	public IndentResult(OtpErlangObject o) {
		if (o instanceof OtpErlangTuple) {
			final OtpErlangTuple t = (OtpErlangTuple) o;
			final OtpErlangLong l0 = (OtpErlangLong) t.elementAt(0);
			final OtpErlangLong l1 = (OtpErlangLong) t.elementAt(1);
			final OtpErlangAtom b = (OtpErlangAtom) t.elementAt(2);
			try {
				indentWith = l0.intValue();
				removeNext = l1.intValue();
			} catch (final OtpErlangRangeException e) {
				indentWith = removeNext = 0;
			}
			addNewLine = b.booleanValue();
		} else {
			addNewLine = false;
		}

	}
}
