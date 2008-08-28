package erlang;

import java.util.List;

import org.eclipse.core.resources.IProject;
import org.erlide.core.builder.BuilderUtils;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.BuildBackend;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlideBuilder {

	public static OtpErlangObject compileYrl(final IProject project,
			final String fn, final String output) {
		try {
			final BuildBackend b = BackendManager.getDefault()
					.getBuild(project);
			final OtpErlangObject r = b.rpcx("erlide_builder", "compile_yrl",
					30000, "ss", fn, output);
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
			final String fn, final String outputdir,
			final List<String> includedirs) {
		try {
			final BuildBackend b = BackendManager.getDefault()
					.getBuild(project);
			// FIXME add an option for the compiler options
			return b.rpcx("erlide_builder", "compile", 20000, "sslsla", fn,
					outputdir, includedirs, new String[] { "debug_info" });
		} catch (final Exception e) {
			ErlLogger.debug(e);
			return null;
		}
	}

	public static OtpErlangList getSourceClashes(final BuildBackend b,
			final String[] dirList) throws ErlangRpcException,
			BackendException, RpcException {
		final OtpErlangList res = (OtpErlangList) b.rpcx("erlide_builder",
				"source_clash", "ls", (Object) dirList);
		return res;
	}

	public static OtpErlangList getCodeClashes(final BuildBackend b)
			throws ErlangRpcException, BackendException, RpcException {
		final OtpErlangList res = (OtpErlangList) b.rpcx("erlide_builder",
				"code_clash", null);
		return res;
	}

	public static void loadModule(final IProject project, final String module) {
		try {
			for (IBackend b : BackendManager.getDefault().getExecution(project)) {
				ErlLogger.debug(":: loading %s in %s", module, b.toString());
				b.rpcx("erlide_builder", "load", "a", module);
			}
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

}
