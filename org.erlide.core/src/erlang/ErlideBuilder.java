package erlang;

import org.eclipse.core.resources.IProject;
import org.erlide.basiccore.ErlLogger;
import org.erlide.core.builder.BuilderUtils;
import org.erlide.core.builder.ErlangBuilder;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.RpcResult;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

public class ErlideBuilder {

	public static OtpErlangObject compileYrl(final IProject project,
			final String fn, final String output) {
		try {
			final RpcResult r = BackendManager.getDefault().get(project).rpct(
					ErlangBuilder.MODULE, "compile_yrl", 30000, "ss", fn, output);
			if (BuilderUtils.isDebugging()) {
				ErlLogger.debug("!!! r== " + r);
			}
			return r.getValue();
		} catch (final Exception e) {
			// e.printStackTrace();
			return null;
		}
	}

	public static OtpErlangObject compileErl(final IProject project,
			final String fn, final String outputdir, final String[] includedirs) {
		try {
			final OtpErlangString[] includes = new OtpErlangString[includedirs.length];
			for (int i = 0; i < includedirs.length; i++) {
				includes[i] = new OtpErlangString(includedirs[i]);
			}
			final OtpErlangList includeList = new OtpErlangList(includes);
			return BackendManager.getDefault().get(project).rpcxt(ErlangBuilder.MODULE,
					"compile", 20000, "ssxx", fn,
					outputdir
					// FIXME add an option for this
					, includeList,
					new OtpErlangList(new OtpErlangAtom("debug_info")));
		} catch (final Exception e) {
			e.printStackTrace();
			return null;
		}
	}

}
