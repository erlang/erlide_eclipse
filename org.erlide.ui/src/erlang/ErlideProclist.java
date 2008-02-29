package erlang;

import org.erlide.runtime.backend.IBackend;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public class ErlideProclist {
	public static final String MODULE_NAME = "erlide_proclist";

	public static void processListInit(IBackend b) {
		try {
			b.rpc(MODULE_NAME, "process_list_init", "");
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}

	public static OtpErlangList getProcessList(IBackend b) {
		try {
			return (OtpErlangList) b.rpcx(MODULE_NAME, "process_list", "");
		} catch (final Exception e) {
			e.printStackTrace();
			return new OtpErlangList();
		}
	}

	public static OtpErlangObject getProcessInfo(IBackend b, OtpErlangPid pid) {
		try {
			return b.rpcx(MODULE_NAME, "get_process_info", "p", pid);
		} catch (final Exception e) {
			e.printStackTrace();
			return new OtpErlangAtom("error");
		}
	}

}
