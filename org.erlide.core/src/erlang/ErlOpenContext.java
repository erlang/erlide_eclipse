package erlang;

import com.ericsson.otp.erlang.OtpErlangList;

public class ErlOpenContext {
	private String externalModules;
	private OtpErlangList pathVars;

	public ErlOpenContext(String externalModules, OtpErlangList pathVars) {
		this.externalModules = externalModules;
		this.pathVars = pathVars;
	}

	public String getExternalModules() {
		return externalModules;
	}

	public OtpErlangList getPathVars() {
		return pathVars;
	}
}