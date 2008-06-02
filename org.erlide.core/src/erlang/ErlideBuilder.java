package erlang;

import org.eclipse.core.resources.IProject;
import org.erlide.basiccore.ErlLogger;
import org.erlide.core.builder.BuilderUtils;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlideBuilder {

	public static OtpErlangObject compileYrl(final IProject project,
			final String fn, final String output) {
		try {
			final OtpErlangObject r = BackendManager.getDefault().get(project)
					.rpcx("erlide_builder", "compile_yrl", 30000, "ss", fn,
							output);
			if (BuilderUtils.isDebugging()) {
				ErlLogger.debug("!!! r== " + r);
			}
			return r;
		} catch (final Exception e) {
			ErlLogger.debug(e);
			return null;
		}
	}

	public static OtpErlangObject compileErl(final IProject project,
			final String fn, final String outputdir, final String[] includedirs) {
		try {
			// final OtpErlangString[] includes = new
			// OtpErlangString[includedirs.length];
			// for (int i = 0; i < includedirs.length; i++) {
			// includes[i] = new OtpErlangString(includedirs[i]);
			// }
			// final OtpErlangList includeList = new OtpErlangList(includes);
			return BackendManager.getDefault().get(project).rpcx(
					"erlide_builder", "compile", 20000, "sslsla", fn,
					outputdir,
					// FIXME add an option for this
					// , includeList,
					includedirs, new String[] { "debug_info" });
			// new OtpErlangList(new OtpErlangAtom("debug_info")));
		} catch (final Exception e) {
			ErlLogger.debug(e);
			return null;
		}
	}

	public static OtpErlangList getSourceClashes(final IBackend b,
			final String[] dirList) throws ErlangRpcException,
			BackendException, RpcException {
		final OtpErlangList res = (OtpErlangList) b.rpcx("erlide_builder",
				"source_clash", "ls", (Object) dirList);
		return res;
	}

	public static OtpErlangList getCodeClashes(final IBackend b)
			throws ErlangRpcException, BackendException, RpcException {
		final OtpErlangList res = (OtpErlangList) b.rpcx("erlide_builder",
				"code_clash", null);
		return res;
	}

	public static OtpErlangObject loadModule(final IProject project,
			final String module) {
		try {
			return BackendManager.getDefault().get(project).rpcx(
					"erlide_builder", "load", "a", module);
		} catch (final Exception e) {
			ErlLogger.debug(e);
			return null;
		}
	}

}
