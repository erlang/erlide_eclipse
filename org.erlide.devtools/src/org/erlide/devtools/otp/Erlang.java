package org.erlide.devtools.otp;

import org.erlide.devtools.Atom;
import org.erlide.devtools.ErlType;
import org.erlide.devtools.Spec;

import com.ericsson.otp.erlang.OtpErlangPid;

public class Erlang {

	@Spec(m = "erlang", f = "now", r = ErlType.TUPLE)
	public int[] now() {
		return null;
	}

	@Spec(m = "erlang", f = "register", r = ErlType.BOOLEAN, a = {
			ErlType.ATOM, ErlType.PID })
	public boolean register(@Atom
	String name, OtpErlangPid pid) {
		return false;
	}

}
